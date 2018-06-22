/*
 * Copyright (C) 2014-2018 Jan Marvin Garbuszus
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
using namespace std;

#include "spss.h"

//' writes the binary SPSS file
//'
//' @param filePath The full systempath to the dta file you want to import.
//' @param dat the data frame
//' @import Rcpp
//' @export
// [[Rcpp::export]]
void writesav(const char * filePath, Rcpp::DataFrame dat)
{
  int32_t k = dat.size();
  int64_t n = dat.nrows();


 fstream sav (filePath, ios::out | ios::binary);
  if (sav.is_open())
  {

    bool swapit = 1;

    int32_t rtype = 0;

    Rcpp::IntegerVector vtyp = dat.attr("vtyp");
    Rcpp::CharacterVector nvarnames = dat.attr("nvarnames");
    std::string timestamp = Rcpp::as<std::string>(dat.attr("timestamp"));
    std::string datestamp = Rcpp::as<std::string>(dat.attr("datestamp"));


    std::string spss = "$FL2@(#)";
    writestr(spss, spss.size(), sav);

    std::string datalabel = "readspss 0.3.5";
    writestr(datalabel, 56, sav);

    int32_t arch = 2;
    writebin(arch, sav, swapit);

    int32_t kk = -1;
    writebin(kk, sav, swapit);

    int32_t cflag=0, cwvariables = 0;

    writebin(cflag, sav, swapit);
    writebin(cwvariables, sav, swapit);

    int64_t nn = -1;
    writebin((int32_t)nn, sav, swapit);

    double bias = 100;
    writebin(bias, sav, swapit);

    writestr(datestamp, datestamp.size(), sav);

    writestr(timestamp, timestamp.size(), sav);

    std::string filelabel (67, ' ');
    writestr(filelabel, filelabel.size(), sav);

    // start variable part
    for (int i = 0; i < k; ++i) {
      rtype = 2;
      writebin(rtype, sav, swapit);

      int32_t subtyp = vtyp[i];
      writebin(subtyp, sav, swapit);

      int32_t vlflag = 0;
      writebin(vlflag, sav, swapit);    // Label flag

      int32_t nmiss = 0;
      writebin(nmiss, sav, swapit);

      int32_t var4;
      char tmp1[4];
      tmp1[0] = 0;
      tmp1[1] = 0;
      tmp1[2] = 0;
      tmp1[3] = 0;
      memcpy(&tmp1[0], &var4, sizeof(var4));
      writebin(var4, sav, 0);

      int32_t var5;
      char tmp2[4];
      tmp2[0] = 0;
      tmp2[1] = 0;
      tmp2[2] = 0;
      tmp2[3] = 0;
      memcpy(&tmp2[0], &var5, sizeof(var5));
      writebin(var5, sav, 0);

      std::string nvarname = Rcpp::as<std::string>(nvarnames[i]);
      writestr(nvarname, 8, sav);

    }

    rtype = 999;

    writebin(rtype, sav, swapit);

    int32_t unk8 = 0;
    writebin(unk8, sav, swapit);

    if (cflag) {
      Rcout << "not yet implemented" << std::endl;
    } else {

      for (int64_t i = 0; i < n; ++i) {
        for (int32_t j = 0; j < k; ++j) {

          int32_t const type = vtyp[j];

          // Rprintf("k: %d; n: %d\n", j, i);
          //
          // Rprintf("vtyp: %d\n", type);


          switch(type)
          {

          case 0:
          {
            double val_d = Rcpp::as<Rcpp::NumericVector>(dat[j])[i];
            writebin(val_d, sav, swapit);
            break;
          }

          default:
          {

            // double len = type;
            //
            // len = ceil(len/8) * 8;
            //
            // std::string val_s ((int32_t)len, '\0');
            // readstring(val_s, sav, val_s.size());
            //
            // // shorten the string to the actual size reported by SPSS
            // val_s.erase(type, std::string::npos);

            break;
          }

          }
        }
      }
    }

    sav.close();
  }

}
