/*
 * Copyright (C) 2014-2019 Jan Marvin Garbuszus
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

using namespace Rcpp;

#include "spss.h"

void write_data(Rcpp::DataFrame dat, int32_t cflag,
                int64_t n, int32_t kk,
                Rcpp::IntegerVector vtyp, Rcpp::IntegerVector itc,
                Rcpp::IntegerVector cc, std::fstream& sav, bool swapit) {

  if (cflag) {

    // data is read in 8 byte chunks. k*n/8 (data remains)
    double chunk = 0;

    int8_t iter = 0;


    unsigned char chnk[8] = "";

    // set chnk to 0
    for (int8_t itr = 0; itr < 8; ++itr) {
      chnk[itr] = 0;
    }

    uint8_t val_b = 0;
    int32_t val_i = 0;
    // double val_d = 0.0;


    std::vector<double> buf_d;
    std::vector<std::string> buf_s;
    std::vector<int> flush_type(8);

    for (int64_t i = 0; i < n; ++i) {
      for (int32_t j = 0; j < kk; ++j) {


        int32_t const type = vtyp[j];
        int32_t const ITC = itc[j];
        int32_t const CC = cc[j];

        // Rprintf("n %d & k %d\n", i, j);
        // Rprintf("iter: %d\n", iter);

        // compression val_b
        if ((type == 0) & (ITC == 1) & (CC == 0)) {
          flush_type[iter] = 0;

          val_i =  Rcpp::as<Rcpp::IntegerVector>(dat[j])[i];

          // Rprintf("val_i: %d\n", val_i);

          val_b = val_i + 100; // add bias

          if (val_i == NA_INTEGER)
            val_b = 255;

          chnk[iter] = val_b;

          ++iter;
        }

        // write compressed
        if ((type == 0)  & (ITC == 0) & (CC == 0)) {

          const double val_d = Rcpp::as<Rcpp::NumericVector>(dat[j])[i];


          flush_type[iter] = 0;
          chnk[iter] = 255;

          if (!( (R_IsNA(val_d)) | R_IsNaN(val_d) | std::isinf(val_d) )) {
            buf_d.push_back(val_d);
            flush_type[iter] = 1;
            chnk[iter] = 253;
          }

          ++iter;
        }

        // strings
        if ((type >= 0)  & (ITC == 0) & (CC == 1)) {
          // Rcout << "--- string ---" << std::endl;


          std::string val_s = as<std::string>(as<CharacterVector>(dat[j])[i]);

          int strlen = type;
          if (strlen == 255) strlen = 256;


          val_s.resize(strlen, ' ');

          // begin writing of the string

          int8_t fills = strlen/8;

          // Rprintf("type: %d\n", type);
          // Rprintf("fills: %d\n", fills);

          int totiter = 0;

          while (fills > 0) {

            // Rprintf("fills: %d\n", fills);
            // Rprintf("iter: %d\n", iter);

            // fill the chunk
            while (iter < 8) {

              int pos = totiter * 8;

              int strt = 253, strl = 8;

              buf_s.push_back(val_s.substr(pos, strl));
              flush_type[iter] = 2;

              chnk[iter] = strt;

              ++iter;
              ++totiter;
              --fills;

              // Rprintf("iter: %d\n", iter);
              if (fills == 0)
                break;
            }

            // Rprintf("fills: %d\n", fills);

            // after a full cicle set iter to 0
            if (iter == 8) {
              // Rcout << "--- iter == 8 ---" << std::endl;

              // Rcout << "full chunk: writing" << std::endl;
              std::memcpy(&chunk, chnk, sizeof(double));
              writebin(chunk, sav, swapit);
              iter = 0;

              int di = 0, ds = 0;
              for (auto flush = 0; flush < 8; ++flush) {

                int ft = flush_type[flush];
                // Rprintf("%d: ft: %d\n", flush, ft);

                if (ft == 1) {
                  double val_d = buf_d[di];
                  writebin(val_d, sav, swapit);
                  ++di;
                }
                if (ft == 2) {
                  std::string vs = buf_s[ds];
                  writestr(vs, 8, sav);
                  ++ds;
                }

              }

              buf_d.clear();
              buf_s.clear();
              flush_type = {0, 0, 0, 0, 0, 0, 0, 0};

            }
          }

        }

        // write chunk of eight and clear chnk
        if (iter == 8) {
          // Rcout << "--- iter == 8 ---" << std::endl;

          std::memcpy(&chunk, chnk, sizeof(double));
          writebin(chunk, sav, swapit);
          iter = 0;

          int di = 0, ds = 0;
          for (auto flush = 0; flush < 8; ++flush) {

            int ft = flush_type[flush];
            // Rprintf("%d: ft: %d\n", flush, ft);

            if (ft == 1) {
              double val_d = buf_d[di];
              writebin(val_d, sav, swapit);
              ++di;
            }
            if (ft == 2) {
              std::string vs = buf_s[ds];
              writestr(vs, 8, sav);
              ++ds;
            }

          }

          buf_d.clear();
          buf_s.clear();
          flush_type = {0, 0, 0, 0, 0, 0, 0, 0};

          // reset chnk
          for (int8_t itr = 0; itr < 8; ++itr) {
            chnk[itr] = 0;
          }

        }

        // write end of file
        if ((i == n-1) & (j == kk -1)) {

          // Rcout << "--- EOF ---" << std::endl;
          // Rprintf("buf_s.size() = %d\n", buf_s.size());
          // Rprintf("buf_d.size() = %d\n", buf_d.size());


          // Rprintf("iter: %d\n", iter);

          // chunk is not yet completely written.
          if (iter > 0) {

            for (int8_t itr = iter; itr < 8; ++itr) {
              chnk[itr] = 0;
            }

            std::memcpy(&chunk, chnk, sizeof(double));
            writebin(chunk, sav, swapit);

            iter = 0;
          }


          int di = 0, ds = 0;
          for (auto flush = 0; flush < 8; ++flush) {

            int ft = flush_type[flush];
            // Rprintf("%d: ft: %d\n", flush, ft);

            if (ft == 1) {
              double val_d = buf_d[di];
              writebin(val_d, sav, swapit);
              ++di;
            }
            if (ft == 2) {
              std::string vs = buf_s[ds];
              writestr(vs, 8, sav);
              ++ds;
            }

          }


          // write EOF
          val_b = 252;
          chnk[iter] = val_b;
          ++iter;

          if (cflag != 2) /* not for zsav */ {
            for (int8_t itr = iter; itr < 8; ++itr) {
              chnk[itr] = 0;
            }

            std::memcpy(&chunk, chnk, sizeof(double));

            writebin(chunk, sav, swapit);
          }
        }

      }
    }


  } else {

    for (int64_t i = 0; i < n; ++i) {
      for (int32_t j = 0; j < kk; ++j) {

        const int32_t type = vtyp[j];

        // Rprintf("k: %d; n: %d\n", j, i);
        //
        // Rprintf("vtyp: %d\n", type);


        switch(type)
        {

        case 0:
        {
          double val_d = Rcpp::as<Rcpp::NumericVector>(dat[j])[i];

          if ( (val_d == NA_REAL) | R_IsNA(val_d) | R_IsNaN(val_d) | std::isinf(val_d) )
            val_d = -DBL_MAX;

          writebin(val_d, sav, swapit);
          break;
        }

        default:
        {

          CharacterVector cv_s = NA_STRING;
          cv_s = as<CharacterVector>(dat[j])[i];

          std::string val_s = "";

          if (cv_s[0] != NA_STRING)
            val_s = as<std::string>(cv_s);

          int size = type;
          if (size == 255)
            size += 1;

          val_s.resize(size, ' ');

          writestr(val_s, val_s.size(), sav);
          break;
        }

        }
      }
    }
  }
}
