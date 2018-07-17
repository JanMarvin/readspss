/*
 * Copyright (C) 2018 Jan Marvin Garbuszus
 *
 * zlib header information by Evan Miller
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

#include <fstream>
#include <string>
#include <zlib.h>

#include "spss.h"

std::string read_sav_uncompress (std::istream& sav,
                                 const bool swapit, const uint8_t cflag,
                                 bool debug) {
  uint64_t zhead_ofs = 0, ztail_ofs = 0, ztail_len = 0;

  int64_t bias = 0;
  int64_t zero = 0;
  int32_t block_size = 0;
  int32_t n_blocks = 0;

  int64_t uncompr_ofs = 0;
  int64_t compr_ofs = 0;
  int32_t uncompr_size = 0;
  int32_t compr_size = 0;

  // read zheader
  zhead_ofs = readbin(zhead_ofs, sav, swapit);
  ztail_ofs = readbin(ztail_ofs, sav, swapit);
  ztail_len = readbin(ztail_len, sav, swapit);

  size_t curpos = sav.tellg();
  sav.seekg(ztail_ofs, std::ios_base::beg);

  // read ztrailer
  bias = readbin(bias, sav, swapit);
  zero = readbin(zero, sav, swapit);
  block_size = readbin(block_size, sav, swapit);
  n_blocks = readbin(n_blocks, sav, swapit);

  // read uncompr and compr ofset and size
  uncompr_ofs   = readbin(uncompr_ofs, sav, swapit);
  compr_ofs     = readbin(compr_ofs, sav, swapit);
  uncompr_size  = readbin(uncompr_size, sav, swapit);
  compr_size    = readbin(compr_size, sav, swapit);

  // // debug
  // Rprintf("1: %d\n", zheader_ofs);
  // Rprintf("2: %d\n", ztrailer_ofs);
  // Rprintf("3: %d\n", ztrailer_len);
  // Rprintf("4: %d\n", n_blocks);
  //
  // Rprintf("1: %d\n", uncompr_ofs);
  // Rprintf("2: %d\n", compr_ofs);
  // Rprintf("3: %d\n", uncompr_size);
  // Rprintf("4: %d\n", compr_size);

  // seek to compr ofset
  sav.seekg(compr_ofs, std::ios_base::beg);

  // Bytef is unsigned char *
  Bytef compr_block[compr_size];
  Bytef uncompr_block[uncompr_size];

  // read the complete compr data part
  sav.read((char*)compr_block, compr_size);

  int32_t status = 0;
  uLong uncompr_block_len = uncompr_size;
  uLong compr_block_len = compr_size;

  // uncompress should be 0
  status = uncompress(uncompr_block, &uncompr_block_len,
                      compr_block, compr_block_len);

  // write to temporary file
  const std::string tempstr = std::tmpnam(nullptr);

  std::fstream outfile (tempstr, std::ios::out | std::ios::binary);
  outfile.write((char *)(&uncompr_block[0]), uncompr_block_len);
  outfile.close();

  return tempstr;
}
