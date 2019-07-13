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
#include <locale>

using namespace Rcpp;

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


  std::ofstream por(filePath, std::ofstream::out | std::ofstream::binary);
  if (por.is_open())
  {

    bool debug = false;


    Rcpp::CharacterVector nvarnames = dat.attr("nvarnames");
    Rcpp::IntegerVector vartyp = dat.attr("vartyp");
    Rcpp::IntegerVector vartypes = dat.attr("vartypes");
    Rcpp::IntegerVector vtyp = dat.attr("vtyp");
    std::string timestamp = Rcpp::as<std::string>(dat.attr("timestamp"));
    std::string datestamp = Rcpp::as<std::string>(dat.attr("datestamp"));
    std::string toEncoding = Rcpp::as<std::string>(dat.attr("toEncoding"));
    Rcpp::CharacterVector label = dat.attr("label");

    Rcpp::IntegerVector haslabel = dat.attr("haslabel");
    Rcpp::List labtabs = dat.attr("labtab");

    int nolabtab = 0;


    std::string file =
      "ASCII SPSS PORT FILE SCII SPSS PORT FILE"
      "ASCII SPSS PORT FILE SCII SPSS PORT FILE"
      "ASCII SPSS PORT FILE SCII SPSS PORT FILE"
      "ASCII SPSS PORT FILE SCII SPSS PORT FILE"
      "ASCII SPSS PORT FILE SCII SPSS PORT FILE"
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
    file += pnum1(11);
    file += "/";


    for (int i = 0; i < k; ++i) {

      if (debug)
        Rcout << "--- 7 ---" << std::endl;

      file += "7"; //var

      int vartypi = vtyp(i);
      int isdate = vartyp(i);

      std::string nvarname = as<std::string>(nvarnames(i));

      file += pnum1(vartypi);
      file += "/";

      file += writestr(nvarname, 0);

      int pfmt1 = 0, wfmt1 = 0;
      int pfmt2 = 8, wfmt2 = 8;
      int pfmt3 = 0, wfmt3 = 0;

      if (vartypi == 0) {
        pfmt1 = isdate, wfmt1 = isdate;
        pfmt2 = 8, wfmt2 = 8;
        pfmt3 = 2, wfmt3 = 2;
      } else {
        pfmt1 = 1, wfmt1 = 1;
        pfmt2 = 8, wfmt2 = 8;
        pfmt3 = 0, wfmt3 = 0;
      }


      // printfmt
      file += pnum1(pfmt1);
      file += "/";

      file += pnum1(pfmt2);
      file += "/";

      file += pnum1(pfmt3);
      file += "/";

      // writefmt
      file += pnum1(wfmt1);
      file += "/";

      file += pnum1(wfmt2);
      file += "/";

      file += pnum1(wfmt3);
      file += "/";


      if (!Rf_isNull(label) && (Rf_length(label) == k )) {

        if (debug)
          Rcout << "--- C ---" << std::endl;

        file += "C"; //var

        std::string lab = as<std::string>(label(i));

        file += writestr(lab,0);
      }

    }

    if (!Rf_isNull(labtabs) && (Rf_length(labtabs) > 0)) {

      if (debug)
        Rcout << "--- D ---" << std::endl;

      file += "D";

      // labtabnam
      Rcpp::CharacterVector labtabnams = labtabs.attr("names");
      Rcpp::IntegerVector   labtab     = labtabs[nolabtab];
      Rcpp::CharacterVector labtn      = labtab.attr("names");

      const std::string nlabs = as<std::string>(labtabnams[nolabtab]);

      file += pnum1(1); // nolab
      file += "/";
      file += writestr(nlabs, 0); // labelsetnam

      file += pnum1(labtab.size()); // labnums
      file += "/";


      // numerics requires pnum1()
      for (int j = 0; j < labtab.size(); ++j) {

        if (debug) {
          Rcout << labtab(j) << std::endl; // val
          Rcout << labtn(j) << std::endl;  // lab
        }

        file += pnum1(labtab(j));
        file += "/";
        file += writestr(as<std::string>(labtn(j)), 0);

      }

      ++nolabtab;
    }

    // start data part
    file += "F";


    if (debug)
      Rcout << "--- F ---" << std::endl;


    for (int64_t i = 0; i < n; ++i) {
      for (int32_t j = 0; j < k; ++j) {

        Rcpp::checkUserInterrupt();


        int32_t const type = vtyp[j];
        // Rprintf("vtyp: %d\n", type);
        // Rprintf("k: %d; n: %d\n", j, i);

        switch(type)
        {

        case 0:
        {
          double val_d = Rcpp::as<Rcpp::NumericVector>(dat[j])[i];

          // Rcout << pfnum(val_d) << std::endl;

          if ( (R_IsNA(val_d)) | R_IsNaN(val_d) | std::isinf(val_d) ) {
            file += "*.";
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

            std::string val_s = "";

            if (cv_s[0] != NA_STRING)
              val_s = as<std::string>(cv_s);

            file += writestr(val_s, 0);
            break;
          }
        }
      }
    }

    // file = Riconv2(file, toEncoding);

    // end with a "Z" even if the line is already
    // 80 chars long
    if ( file.size() % 80 == 0)
      file += "Z";

    while ( file.size() % 80 != 0)
      file += "Z";


    file = linebreak(file);
    file += "\n";

    // std::locale mylocale("");
    // por.imbue(mylocale);
    por << file;

    por.close();

  }
}
