/*
 * Copyright (C) 2019 Jan Marvin Garbuszus
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
#include <zlib.h>

#include "spss.h"


void write_sav_compress (std::fstream& sav, const std::string tempstr,
                         const bool swapit, bool debug) {

  // open zsav destination
  if (sav.is_open())
  {

    // offset positions
    uint64_t zhead_ofs = 0, ztail_ofs = 0, ztail_len = 0;


    std::fstream tmp (tempstr, std::ios::in | std::ios::binary);
    if (!tmp.is_open()) Rcpp::stop("tmp not open");

    // temporary sav file to be removed afterwards
    tmp.seekg(0, std::ios_base::beg);
    size_t curpos = tmp.tellg();
    tmp.seekg(0, std::ios_base::end);
    size_t savlen = tmp.tellg();
    tmp.seekg(0, std::ios_base::beg);

    int64_t bias = -100, zero = 0;
    int32_t block_size = 4190208; // bytes
    int32_t n_blocks = ceil((double)savlen/block_size);

    if (debug)
      Rcpp::Rcout << savlen << " " << curpos << " " << n_blocks << std::endl;

    int64_t uncompr_ofs = 0, compr_ofs = 0;
    int32_t uncompr_size = block_size, compr_size = compressBound(uncompr_size);

    std::vector<int64_t> u_ofs(n_blocks), c_ofs(n_blocks);
    std::vector<int32_t> u_size(n_blocks), c_size(n_blocks);

    // zlib header

    // write zheader will be replaced with  actual values once they are
    // known after writing the entire file.
    zhead_ofs = sav.tellg(); // is initial u_ofs

    writebin(zhead_ofs, sav, swapit);
    writebin(ztail_ofs, sav, swapit);
    writebin(ztail_len, sav, swapit);

    // compress sav
    for (int32_t i = 0; i < n_blocks; ++i) {

      // modify chunk size for last chunk
      if (i == (n_blocks-1)) {
        int64_t len = savlen - (block_size * (n_blocks -1));

        uncompr_size = len;
        compr_size = compressBound(uncompr_size);
      }

      uncompr_ofs = zhead_ofs;
      if (i > 0) uncompr_ofs = u_ofs[i-1] + u_size[i-1];
      compr_ofs = sav.tellg();

      int32_t status = 0;
      uLong uncompr_block_len = uncompr_size;
      uLong compr_block_len   = compr_size;

      // Bytef is unsigned char *
      std::vector<unsigned char> uncompr_block(uncompr_size);
      std::vector<unsigned char> compr_block(compr_size);

      // read the uncompr data part
      tmp.read((char*)(&uncompr_block[0]), uncompr_size);

      // uncompress should be 0
      status = compress2(&compr_block[0], &compr_block_len,
                         &uncompr_block[0], uncompr_block_len,
                         Z_DEFAULT_COMPRESSION);

      if (status != Z_OK) Rcpp::stop("compression failed.");


      if (debug)
        Rcpp::Rcout << "uncompressed: " << uncompr_block_len <<
          "\ncompressed: " << compr_block_len << std::endl;

      sav.write((char *)(&compr_block[0]), compr_block_len);

      // export
      u_ofs[i] = uncompr_ofs;
      c_ofs[i] = compr_ofs;
      u_size[i] = uncompr_block_len;
      c_size[i] = compr_block_len;
    }

    // zlib trailer

    ztail_ofs = sav.tellg();

    writebin(bias, sav, swapit);
    writebin(zero, sav, swapit);
    writebin(block_size, sav, swapit);
    writebin(n_blocks, sav, swapit);

    // write uncompr and compr ofset and size
    for (int32_t i = 0; i < n_blocks; ++i) {
      writebin(u_ofs[i], sav, swapit);
      writebin(c_ofs[i], sav, swapit);
      writebin(u_size[i], sav, swapit);
      writebin(c_size[i], sav, swapit);
    }

    /* get ztail_len */
    uint64_t ztail_end = sav.tellg();
    ztail_len = ztail_end - ztail_ofs;

    /* write ztail_ofs and ztail_len */
    sav.seekg(zhead_ofs, sav.beg);
    writebin(zhead_ofs, sav, swapit);
    writebin(ztail_ofs, sav, swapit);
    writebin(ztail_len, sav, swapit);


    if(debug) {
      Rcpp::Rcout << "zhead_ofs " << zhead_ofs << "\n" <<
        "ztail_ofs " << ztail_ofs << "\n" <<
          "ztail_len " << ztail_len << "\n" << std::endl;
    }

    tmp.close();

  } else {
    Rcpp::stop("sav file is unexpectedly closed");
  }

}
