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


  sav.seekg(ztail_ofs, std::ios_base::beg);

  // read ztrailer
  bias = readbin(bias, sav, swapit);
  zero = readbin(zero, sav, swapit);
  block_size = readbin(block_size, sav, swapit);
  n_blocks = readbin(n_blocks, sav, swapit);

  std::vector<int64_t> u_ofs(n_blocks), c_ofs(n_blocks);
  std::vector<int32_t> u_size(n_blocks), c_size(n_blocks);


  for (int i = 0; i < n_blocks; ++i) {
    // read uncompr and compr ofset and size
    u_ofs[i]   = readbin(uncompr_ofs, sav, swapit);
    c_ofs[i]     = readbin(compr_ofs, sav, swapit);
    u_size[i]  = readbin(uncompr_size, sav, swapit);
    c_size[i]    = readbin(compr_size, sav, swapit);

    if(debug) {
      Rcpp::Rcout << "uofs " << u_ofs[i] << std::endl;
      Rcpp::Rcout << "cofs " << c_ofs[i] << std::endl;
      Rcpp::Rcout << "usize " << u_size[i] << std::endl;
      Rcpp::Rcout << "csize " << c_size[i] << std::endl;
    }
  }

  if(debug) {
    Rcpp::Rcout << "zhead_ofs " << zhead_ofs <<
      "\nztail_ofs " << ztail_ofs <<
        "\nztail_len " << ztail_len <<
          "\nbias " << bias <<
            "\nzero " << zero <<
              "\nblock_size " << block_size <<
                "\nn_blocks " << n_blocks <<
                  std::endl;
  }


  // write to temporary file
  const std::string tempstr = ".readspss_unc_tmp_file";
  std::fstream outfile (tempstr, std::ios::out | std::ios::binary);

  for (int i = 0; i < n_blocks; ++i) {
    // seek to compr ofset
    sav.seekg(c_ofs[i], std::ios_base::beg);

    // Bytef is unsigned char *
    std::vector<unsigned char>   compr_block(c_size[i], 0);
    std::vector<unsigned char> uncompr_block(u_size[i], 0);

    // read the complete compr data part
    sav.read((char*)&compr_block[0], c_size[i]);

    int32_t status = 0;
    uLong uncompr_block_len = u_size[i];
    uLong compr_block_len = c_size[i];

    // uncompress should be 0
    status = uncompress(&uncompr_block[0], &uncompr_block_len,
                        &compr_block[0], compr_block_len);

    if (status != 0) Rcpp::stop("uncompress failed.");


    outfile.write((char *)(&uncompr_block[0]), uncompr_block_len);
  }
  outfile.close();

  return tempstr;
}
