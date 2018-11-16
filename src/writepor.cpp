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
void writepor(const char * filePath, Rcpp::DataFrame dat)
{

  int32_t k = dat.size();
  int64_t n = dat.nrows();


  fstream por (filePath, ios::out | ios::binary);
  if (por.is_open())
  {


    Rcpp::CharacterVector nvarnames = dat.attr("nvarnames");
    Rcpp::IntegerVector vartypes = dat.attr("vartypes");
    Rcpp::IntegerVector vtyp = dat.attr("vtyp");
    std::string timestamp = Rcpp::as<std::string>(dat.attr("timestamp"));
    std::string datestamp = Rcpp::as<std::string>(dat.attr("datestamp"));


    std::string file =
      "ASCII SPSS PORT FILE                    "
      "ASCII SPSS PORT FILE                    "
      "ASCII SPSS PORT FILE                    "
      "ASCII SPSS PORT FILE                    "
      "ASCII SPSS PORT FILE                    "
      "0000000000000000000000000000000000000000"
      "0000000000000000000000000123456789ABCDEF"
      "GHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrst"
      "uvwxyz .<(+0&[]!$*);^-/|,%_>?`:#@'=\"000"
      "000~000000000000000000000{}\\00000000000"
      "0000000000000000000000000000000000000000"
      "000000000000000000SPSSPORT";


    file += "A"; // vers

    file += writestr(datestamp, 0);

    file += writestr(timestamp, 0);

    file += "1";

    file += writestr("readspss reads and writes por files", 0);

    file += "4"; // varrec
    file += pnum1(k);
    file += "/";

    file += "5"; // prec
    file += "10";
    file += "/";


    for (int i = 0; i < k; ++i) {

      file += "7"; //var

      int vartyp = vartypes(i);

      std::string nvarname = as<std::string>(nvarnames(i));

      file += pnum1(vartyp);
      file += "/";

      file += writestr(nvarname, 0);

      int pfmt1 =0, wfmt1 =0;

      // if (vartyp == 0) {
        pfmt1 = 5, wfmt1 = 5;
      // } else {
      //   pfmt1 = vartyp, wfmt1 = vartyp;
      // }


      // printfmt
      file += pnum1(pfmt1);
      file += "/";

      file += pnum1(8);
      file += "/";

      file += pnum1(2);
      file += "/";

      // writefmt
      file += pnum1(wfmt1);
      file += "/";

      file += pnum1(8);
      file += "/";

      file += pnum1(2);
      file += "/";

    }


    // Rcout << n << " " << k << std::endl;


    // stop("debug");

    // start data part
    file += "F";


    for (int64_t i = 0; i < n; ++i) {
      for (int32_t j = 0; j < k; ++j) {


        int32_t const type = vtyp[j];
        Rprintf("vtyp: %d\n", type);
        Rprintf("k: %d; n: %d\n", j, i);

        switch(type)
        {

        case 0:
        {
          double val_d = Rcpp::as<Rcpp::NumericVector>(dat[j])[i];

          // Rcout << pfnum(val_d) << std::endl;

          if (val_d == NA_REAL) {
            file += "*./";
          } else {
            file += pfnum(val_d);
            file += "/";
          }

          break;
        }

          default:
          {

            CharacterVector cv_s = NA_STRING;
            cv_s = as<CharacterVector>(dat[j])[i];

            string val_s = "";

            if (cv_s[0] != NA_STRING)
              val_s = as<string>(cv_s);

            file += writestr(val_s, 0);
            break;
          }
        }
      }
    }


    // double val_d = 0;
    //
    // std::string val_s;
    // val_s = pfnum(val_d);
    //
    // Rcout << val_s << std::endl;
    //
    // double val_r = 0.0;
    // int mv = 0;
    //
    // dnum(&val_s[0], val_r, &mv);
    //
    // Rcout << val_r << std::endl;
    //
    //
    // int val_i = 251;
    //
    // val_s = "";
    // val_s = pnum1(val_i);
    //
    // Rcout << val_s << std::endl;
    //
    // val_r = 0.0;
    // mv = 0;
    //
    // dnum(&val_s[0], val_r, &mv);
    //
    // Rcout << val_r << std::endl;

    while ( file.size() % 80 != 0)
      file += "Z";

    file = linebreak(file);

    writestr(file, file.size(), por);

  }
}
