/*
 * Copyright (C) 2018 Jan Marvin Garbuszus
 * Copyright (c) 2013 Ben Pfaff
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the
 * Free Software Foundation; either version 2 of the License, or (at your
 * option) any later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for
 * more details.
 *
 * You should have received a copy of the GNU General Public License along
 * with this program. If not, see <http://www.gnu.org/licenses/>.
 */

#include <Rcpp.h>
#include <stdint.h>
#include <string>
#include <fstream>
#include <streambuf>
#include <regex>

#include <openssl/aes.h>
#include <openssl/cmac.h>

using namespace Rcpp;

#include "spss.h"
#include "read_sav_encrypted.h"
#include "readsav.h"


int encryptfile (const char * filePath, std::string &outpath, std::string pass)
{
  uint8_t inblock[36];
  char pw[11];
  AES_KEY aes;

  char *argv = (char *)pass.c_str();

  // currently unused do not know if swapping is required
  // bool swapit = false;


  std::fstream sav(filePath, std::ios::in | std::ios::binary);
  if (sav) {

    std::string fileheader(36, '\0');
    fileheader = readstring(fileheader, sav);

    if (!std::regex_search(fileheader, std::regex("ENCRYPTEDSAV"))) {
      stop("The file header indicates that it is not an SPSS sav file.");
    }



    /* Read first ciphertext block and use it to verify the password.  Try the
     password as plaintext first, then try decoding it. */

    sav.read((char*)inblock, 16);

    if (!init (argv, inblock, &aes)
          && !(decode_password (argv, pw) && init (pw, inblock, &aes)))
    {
      Rprintf ("wrong password, sorry\n");
      return (1);
    } else {
      // rewind so that a full sav file is returned
      sav.seekg(36, std::ios_base::beg);
    }

    // file is written into a temp file
    const std::string tempstr = ".readspss_enc_tmp_file";
    std::fstream outfile (tempstr, std::ios::out | std::ios::binary);

    /* Decrypt entire input. */
    while (sav.read ((char*)inblock, 16))
    {
      uint8_t outblock[16];

      AES_ecb_encrypt (inblock, outblock, &aes, AES_DECRYPT);
      outfile.write((char *)(&outblock[0]), 16);
    }
    outfile.close();

    // this is returned as this file is read
    outpath = tempstr;

    return 0;

  } else {
    return -1;
  }

}


//' Read encrypted SPSS file
//'
//' @param filePath The full systempath to the dta file you want to import.
//' @param debug print debug information
//' @param encStr encoding string
//' @param ownEnc encoding provided by localeToCharset
//' @param pass passkey required for encoding
//' @import Rcpp
//' @keywords internal
//' @noRd
// [[Rcpp::export]]
Rcpp::List readencrypted(const char * filePath, const bool debug,
                         std::string encStr,
                         std::string const ownEnc,
                         std::string const pass) {

  std::string outPath;
  Rcpp::List df;

  // encrypt the sav-file
  if (encryptfile(filePath, outPath, pass) == 0) {
    df = readsav(outPath.c_str(), debug, encStr, ownEnc);

    // remove encrypted sav-file
    std::remove(outPath.c_str());
  } else {
    stop("stopping");
  }

  return df;
}
