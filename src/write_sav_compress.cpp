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


void write_sav_compress (std::fstream& sav, std::string tempstr,
                         const bool swapit, bool debug) {



  std::fstream tmp (tempstr, std::ios::in | std::ios::binary);

  // open zsav destination
  if (sav.is_open())
  {

    // offset positions
    uint64_t zhead_ofs = 0, ztail_ofs = 0, ztail_len = 0;

    // temporary sav file to be removed afterwards

    tmp.seekg(0, tmp.beg);
    auto curpos = tmp.tellg();
    tmp.seekg(0, tmp.end);
    auto savlen = tmp.tellg();
    tmp.seekg(0, tmp.beg);

    int64_t bias = -100, zero = 0;
    int32_t block_size = 4190208; // bytes
    int32_t n_blocks = ceil((double)savlen/block_size);

    if (debug)
      Rcpp::Rcout << savlen << " " << curpos <<
        " " << savlen << " " << n_blocks << std::endl;

    int64_t uncompr_ofs = 0, compr_ofs = 0;
    int32_t uncompr_size = 0, compr_size = 0;

    uncompr_size = savlen;
    compr_size = compressBound(savlen);

    std::vector<int64_t> u_ofs(n_blocks), c_ofs(n_blocks);
    std::vector<int32_t> u_size(n_blocks, uncompr_size),
                         c_size(n_blocks, compr_size);

    // zlib header

    // write zheader will be replaced with  actual values once they are
    // known after writing the entire file.
    zhead_ofs = sav.tellg(); // is initial u_ofs

    writebin(zhead_ofs, sav, swapit);
    writebin(ztail_ofs, sav, swapit);
    writebin(ztail_len, sav, swapit);


    // compress sav
    for (int i = 0; i < n_blocks; ++i) {

      uncompr_ofs = zhead_ofs;
      compr_ofs = sav.tellg();

      // Bytef is unsigned char *
      std::vector<unsigned char> uncompr_block(block_size);
      std::vector<unsigned char>   compr_block(block_size);

      int32_t status = 0;
      uLong uncompr_block_len = uncompr_size;
      uLong compr_block_len   = compr_size;

      // read the uncompr data part
      tmp.read((char*)(&uncompr_block[0]), block_size);

      // uncompress should be 0
      status = compress2(&compr_block[0], &compr_block_len,
                         &uncompr_block[0], uncompr_block_len,
                         Z_DEFAULT_COMPRESSION);

      if (status != 0) Rcpp::stop("compression failed.");


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
    for (int i = 0; i < n_blocks; ++i) {
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

    // sav file is complete
    tmp.close();
  }

}
