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


#include <Rcpp.h>
#include <stdint.h>
#include <string>
#include <fstream>
#include <streambuf>
#include <zlib.h>

#include "spss.h"


void write_sav_compress (std::fstream& zsav, std::fstream& sav,
                         const bool swapit, bool debug) {

  // open zsav destination
  if (zsav.is_open())
  {

    // offset positions
    uint64_t zhead_ofs = 0, ztail_ofs = 0, ztail_len = 0;

    // temporary sav file to be removed afterwards

    sav.seekg(0, sav.beg);
    auto curpos = sav.tellg();
    sav.seekg(0, sav.end);
    auto savlen = sav.tellg();
    sav.seekg(0, sav.beg);

    int64_t bias = -100, zero = 0;
    int32_t block_size = 4190208;
    int32_t n_blocks = ceil((double)savlen/block_size);

    if (debug)
      Rcpp::Rcout << savlen << " " << curpos <<
        " " << savlen << " " << n_blocks << std::endl;

    int64_t uncompr_ofs = 0, compr_ofs = 0;
    int32_t uncompr_size = 0, compr_size = 0;

    uncompr_size = savlen; compr_size = compressBound(savlen);

    std::vector<int64_t> u_ofs(n_blocks), c_ofs(n_blocks);
    std::vector<int32_t> u_size(n_blocks, uncompr_size), c_size(n_blocks, compr_size);

    // zlib header

    // write zheader will be replaced with  actual values once they are
    // known after writing the entire file.
    zhead_ofs = zsav.tellg(); // is initial u_ofs

    writebin(zhead_ofs, zsav, swapit);
    writebin(ztail_ofs, zsav, swapit);
    writebin(ztail_len, zsav, swapit);

    // compress sav
    // for (int i = 0; i < n_blocks; ++i) {
      // seek to compr ofset
      uncompr_ofs = zhead_ofs;
      compr_ofs = zsav.tellg();

      // Bytef is unsigned char *
      std::vector<unsigned char> uncompr_block(uncompr_size, 0);
      std::vector<unsigned char>   compr_block(  compr_size, 0);

      // read the uncompr data part
      sav.read((char*)(&uncompr_block[0]), uncompr_size);

      int32_t status = 0;
      uLong uncompr_block_len = uncompr_size;
      uLong compr_block_len   = compr_size;

      // uncompress should be 0
      status = compress2(&compr_block[0], &compr_block_len,
                         &uncompr_block[0], uncompr_block_len,
                         1); /* zlib header 78 01 */

      if (status != 0)
        Rcpp::stop("compression failed.");

      Rcpp::Rcout << compr_size << " vs " << compr_block_len << std::endl;

      zsav.write((char *)(&compr_block[0]), compr_block_len);
    // }

    // zlib trailer

    ztail_ofs = zsav.tellg();

    writebin(bias, zsav, swapit);
    writebin(zero, zsav, swapit);
    writebin(block_size, zsav, swapit);
    writebin(n_blocks, zsav, swapit);

    // write uncompr and compr ofset and size
    // for (int i = 0; i < n_blocks; ++i) {
      writebin(uncompr_ofs, zsav, swapit);
      writebin(compr_ofs, zsav, swapit);
      writebin((int32_t)uncompr_block_len, zsav, swapit);
      writebin((int32_t)compr_block_len, zsav, swapit);
    // }

    /* get ztail_len */
    uint64_t ztail_end = zsav.tellg();
    ztail_len = ztail_end - ztail_ofs;

    Rcpp::Rcout << ztail_end << std::endl;

    /* write ztail_ofs and ztail_len */
    zsav.seekg(zhead_ofs, zsav.beg);
    writebin(zhead_ofs, zsav, swapit);
    writebin(ztail_ofs, zsav, swapit);
    writebin(ztail_len, zsav, swapit);


    if(debug) {
      Rcpp::Rcout << "zhead_ofs " << zhead_ofs << "\n" <<
        "ztail_ofs " << ztail_ofs << "\n" <<
          "ztail_len " << ztail_len << "\n" << std::endl;
    }

    // zsav file is complete
    // zsav.close();
  }

}
