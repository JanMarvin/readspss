/*
 * Copyright (c) 2013 Ben Pfaff.
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to
 * deal in the Software without restriction, including without limitation the
 * rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
 * sell copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
 * IN THE SOFTWARE.
 */

#include <assert.h>
#include <stdint.h>
#include <string.h>

#include <openssl/aes.h>
#include <openssl/cmac.h>


/* Initializes AES from PASSWORD.  Returns true if CIPHERTEXT is the first
 ciphertext block in an encrypted .sav file for PASSWORD, false if PASSWORD
 is wrong. */
static bool
init(const char *password, const uint8_t ciphertext[16], AES_KEY *aes)
{
  /* NIST SP 800-108 fixed data. */
  static const uint8_t fixed[] = {
    /* i */
    0x00, 0x00, 0x00, 0x01,

    /* label */
    0x35, 0x27, 0x13, 0xcc, 0x53, 0xa7, 0x78, 0x89,
    0x87, 0x53, 0x22, 0x11, 0xd6, 0x5b, 0x31, 0x58,
    0xdc, 0xfe, 0x2e, 0x7e, 0x94, 0xda, 0x2f, 0x00,
    0xcc, 0x15, 0x71, 0x80, 0x0a, 0x6c, 0x63, 0x53,

    /* delimiter */
    0x00,

    /* context */
    0x38, 0xc3, 0x38, 0xac, 0x22, 0xf3, 0x63, 0x62,
    0x0e, 0xce, 0x85, 0x3f, 0xb8, 0x07, 0x4c, 0x4e,
    0x2b, 0x77, 0xc7, 0x21, 0xf5, 0x1a, 0x80, 0x1d,
    0x67, 0xfb, 0xe1, 0xe1, 0x83, 0x07, 0xd8, 0x0d,

    /* L */
    0x00, 0x00, 0x01, 0x00,
  };

  char padded_password[32];
  uint8_t plaintext[16];
  size_t password_len;
  uint8_t cmac[16];
  uint8_t key[32];
  size_t cmac_len;
  CMAC_CTX *ctx;
  int retval;

  /* Truncate password to at most 10 bytes. */
  password_len = strlen (password);
  if (password_len > 10)
    password_len = 10;

  /* padded_password = password padded with zeros to 32 bytes. */
  memset (padded_password, 0, sizeof padded_password);
  memcpy (padded_password, password, password_len);

  /* cmac = CMAC(padded_password, fixed). */
  ctx = CMAC_CTX_new ();
  assert (ctx != NULL);

  retval = CMAC_Init (ctx, padded_password, sizeof padded_password,
                      EVP_aes_256_cbc (), NULL);
  assert (retval == 1);

  retval = CMAC_Update (ctx, fixed, sizeof fixed);
  assert (retval == 1);

  cmac_len = sizeof cmac;
  retval = CMAC_Final (ctx, cmac, &cmac_len);
  assert (retval == 1);
  assert (cmac_len == 16);

  /* The key is the cmac repeated twice. */
  memcpy(key, cmac, 16);
  memcpy(key + 16, cmac, 16);

  /* Use key to initialize AES. */
  assert (sizeof key == 32);
  retval = AES_set_decrypt_key (key, sizeof key * 8, aes);
  assert (retval >= 0);

  /* Check for magic number "$FL" always present in SPSS .sav file. */
  AES_ecb_encrypt (ciphertext, plaintext, aes, AES_DECRYPT);
  return !memcmp (plaintext, "$FL", 3);
}

/* Password decoding. */

#define b(x) (1 << (x))

static const uint16_t m0[4][2] = {
  { b(2),                         b(2) | b(3) | b(6) | b(7) },
  { b(3),                         b(0) | b(1) | b(4) | b(5) },
  { b(4) | b(7),                  b(8) | b(9) | b(12) | b(14) },
  { b(5) | b(6),                  b(10) | b(11) | b(14) | b(15) },
};

static const uint16_t m1[4][2] = {
  { b(0) | b(3) | b(12) | b(15),  b(0) | b(1) | b(4) | b(5) },
  { b(1) | b(2) | b(13) | b(14),  b(2) | b(3) | b(6) | b(7) },
  { b(4) | b(7) | b(8) | b(11),   b(8) | b(9) | b(12) | b(13) },
  { b(5) | b(6) | b(9) | b(10),   b(10) | b(11) | b(14) | b(15) },
};

static const uint16_t m2[4][2] = {
  { b(2),                         b(1) | b(3) | b(9) | b(11) },
  { b(3),                         b(0) | b(2) | b(8) | b(10) },
  { b(4) | b(7),                  b(4) | b(6) | b(12) | b(14) },
  { b(5) | b(6),                  b(5) | b(7) | b(13) | b(15) },
};

static const uint16_t m3[4][2] = {
  { b(0) | b(3) | b(12) | b(15),  b(0) | b(2) | b(8) | b(10) },
  { b(1) | b(2) | b(13) | b(14),  b(1) | b(3) | b(9) | b(11) },
  { b(4) | b(7) | b(8) | b(11),   b(4) | b(6) | b(12) | b(14) },
  { b(5) | b(6) | b(9) | b(10),   b(5) | b(7) | b(13) | b(15) },
};

static int
  decode_nibble (const uint16_t table[4][2], int nibble)
  {
    int i;

    for (i = 0; i < 4; i++) {
      if (table[i][0] & (1 << nibble))
        return table[i][1];
    }

      return 0;
  }

/* Returns true if X has exactly one 1-bit, false otherwise. */
static bool
  is_pow2 (int x)
  {
    return x && (x & (x - 1)) == 0;
  }

/* If X has exactly one 1-bit, returns its index, where bit 0 is the LSB.
 Otherwise, returns 0. */
static int
  find_1bit (uint16_t x)
  {
    int i;

    if (!is_pow2 (x))
      return -1;

    for (i = 0; i < 16; i++)
      if (x & (1u << i))
        return i;

    // hopefully never reachead. was abort(), but
    // check() does not like that
    return 0;
  }

/* Attempts to decode a pair of encoded password characters A and B into a
 single byte of the plaintext password.  Returns 0 if A and B are not a valid
 encoded password pair, otherwise a byte of the plaintext password. */
static int
  decode_password_2bytes (uint8_t a, uint8_t b)
  {
    int x = find_1bit (decode_nibble (m0, a >> 4) & decode_nibble (m2, b >> 4));
    int y = find_1bit (decode_nibble (m1, a & 15) & decode_nibble (m3, b & 15));
    return x < 0 || y < 0 ? 0 : (x << 4) | y;
  }

/* Decodes an SPSS so-called "encrypted" password INPUT into OUTPUT.

 An encoded password is always an even number of bytes long and no longer
 than 20 bytes.  A decoded password is never longer than 10 bytes plus a null
 terminator.

 Returns true if successful, otherwise false. */
static bool
  decode_password (const char *input, char output[11])
  {
    size_t len;

    len = strlen (input);
    if (len > 20 || len % 2)
      return false;

    for (; *input; input += 2)
    {
      int c = decode_password_2bytes (input[0], input[1]);
      if (!c)
        return false;
      *output++ = c;
    }
    *output = '\0';

    return true;
  }
