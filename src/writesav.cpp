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
//' @param compress the file
//' @import Rcpp
//' @export
// [[Rcpp::export]]
void writesav(const char * filePath, Rcpp::DataFrame dat, uint8_t compress)
{
  int32_t k = dat.size();
  int64_t n = dat.nrows();


  fstream sav (filePath, ios::out | ios::binary);
  if (sav.is_open())
  {

    bool swapit = 0;

    int32_t rtype = 0, subtyp = 0, size = 0, count = 0;
    std::string empty = "";

    Rcpp::IntegerVector vtyp = dat.attr("vtyp");
    Rcpp::IntegerVector cc = dat.attr("cc");
    Rcpp::IntegerVector itc = dat.attr("itc");
    Rcpp::IntegerVector vartypes = dat.attr("vartypes");

    Rcpp::CharacterVector nvarnames = dat.attr("nvarnames");
    Rcpp::CharacterVector label = dat.attr("label");

    Rcpp::IntegerVector haslabel = dat.attr("haslabel");
    Rcpp::List labtab = dat.attr("labtab");

    std::string timestamp = Rcpp::as<std::string>(dat.attr("timestamp"));
    std::string datestamp = Rcpp::as<std::string>(dat.attr("datestamp"));
    std::string longvarname = Rcpp::as<std::string>(dat.attr("longvarnames"));



    std::string spss = "$FL2@(#)";
    writestr(spss, spss.size(), sav);

    std::string datalabel = "readspss 0.3.5";
    writestr(datalabel, 56, sav);

    int32_t arch = 2;
    writebin(arch, sav, swapit);

    // int32_t kk = -1;
    writebin(k, sav, swapit);

    int32_t cflag=0, cwvariables = 0;

    if (compress)
      cflag = 1;

    writebin(cflag, sav, swapit);
    writebin(cwvariables, sav, swapit);

    // int64_t nn = -1;
    writebin((int32_t)n, sav, swapit);

    double bias = 100;
    writebin(bias, sav, swapit);

    writestr(datestamp, datestamp.size(), sav);

    writestr(timestamp, timestamp.size(), sav);

    std::string filelabel (67, ' ');
    writestr(filelabel, filelabel.size(), sav);

    // rtype 2 -----------------------------------------------------------------
    // start variable part
    for (int i = 0; i < vartypes.size(); ++i) {
      rtype = 2;
      writebin(rtype, sav, swapit);

      int32_t subtyp = vartypes[i];
      writebin(subtyp, sav, swapit);

      int32_t vlflag = 0;

      if (dat.size() == label.size())
        vlflag = 1;

      writebin(vlflag, sav, swapit);    // Label flag

      int32_t nmiss = 0;
      writebin(nmiss, sav, swapit);

      int32_t var4;
      char tmp1[4];
      if (subtyp == 0) {
        tmp1[0] = 2;
        tmp1[1] = 8;
        tmp1[2] = 5;
        tmp1[3] = 0;
      } else if (subtyp > 0) {
        tmp1[0] = 0;
        tmp1[1] = subtyp;
        tmp1[2] = 1;
        tmp1[3] = 0;
      } else if (subtyp == -1) {
        tmp1[0] = 0;
        tmp1[1] = 0;
        tmp1[2] = 0;
        tmp1[3] = 0;
      }

      var4 = ((int8_t)tmp1[3] << 24) | ((int8_t)tmp1[2] << 16) |
        ((int8_t)tmp1[1] << 8) | (int8_t)tmp1[0];
      writebin(var4, sav, 0);

      int32_t var5;
      char tmp2[4];
      if (subtyp == 0) {
        tmp2[0] = 2;
        tmp2[1] = 8;
        tmp2[2] = 5;
        tmp2[3] = 0;
      } else if (subtyp > 0) {
        tmp2[0] = 0;
        tmp2[1] = subtyp;
        tmp2[2] = 1;
        tmp2[3] = 0;
      } else if (subtyp == -1) {
        tmp2[0] = 0;
        tmp2[1] = 0;
        tmp2[2] = 0;
        tmp2[3] = 0;
      }

      var5 = ((int8_t)tmp2[3] << 24) | ((int8_t)tmp2[2] << 16) |
        ((int8_t)tmp2[1] << 8) | (int8_t)tmp2[0];
      writebin(var5, sav, 0);

      std::string nvarname = Rcpp::as<std::string>(nvarnames[i]);
      writestr(nvarname, 8, sav);

      if (vlflag == 1) {

        std::string lab = Rcpp::as<std::string>(label[i]);

        int32_t origlen = 0;
        origlen = lab.size();
        origlen = ceil((double)origlen/4) * 4;

        writebin(origlen, sav, swapit);
        writestr(lab, origlen, sav);
      }

    }

    if(!Rf_isNull(haslabel))
    {

      // rtype 3 ---------------------------------------------------------------

      int32_t nolabels = haslabel.size();

      for (int i = 0; i < nolabels; ++i) {

        rtype = 3;
        writebin(rtype, sav, swapit);

        Rcpp::IntegerVector code = labtab(i);

        std::vector<std::string> labs = code.attr("names");

        int32_t nolab = code.size();
        writebin(nolab, sav, swapit);

        for (int j = 0; j < nolab; ++j) {

          double coden = code[j];
          std::string lab = labs[j];

          writebin(coden, sav, swapit);
          uint8_t lablen = lab.size();
          if (lablen > 120) {
            lablen = 120;
            warning("Label longer than 120 characters found. Trimmed to 120.");
          }

          writebin(lablen, sav, swapit);

          lablen = ( ceil((double)(lablen+1)/8) * 8 ) - 1;
          writestr(lab, lablen, sav);

        }


        // rtype 4 -------------------------------------------------------------

        rtype = 4;
        writebin(rtype, sav, swapit);


        // if multipe variables share a single value this will be a vector
        int32_t nolabel = 1;

        writebin(nolabel, sav, swapit);


        int32_t lab_id = 0;

        lab_id = haslabel[i];

        writebin(lab_id, sav, swapit);
      }

    }



    // rtype 7 -----------------------------------------------------------------

    if (longvarname.compare(empty) != 0) {
      // beign longvarnames
      rtype = 7;
      writebin(rtype, sav, swapit);

      subtyp = 13;
      writebin(subtyp, sav, swapit);

      size = 1;
      writebin(size, sav, swapit);

      count = longvarname.size();
      writebin(count, sav, swapit);

      writestr(longvarname, longvarname.size(), sav);
      // end longvarnames
    }

    rtype = 999;

    writebin(rtype, sav, swapit);

    int32_t unk8 = 0;
    writebin(unk8, sav, swapit);

    if (cflag) {

      // data is read in 8 byte chunks. k*n/8 (data remains)
      double chunk = 0;

      int8_t iter = 0;


      unsigned char chnk[8];

      // set chnk to 0
      for (int8_t itr = 0; itr < 8; ++itr) {
        chnk[itr] = 0;
      }

      uint8_t val_b = 0;
      int16_t val_i = 0;
      double val_d = 0.0;


      for (int64_t i = 0; i < n; ++i) {
        for (int32_t j = 0; j < k; ++j) {


          int32_t const type = vtyp[j];
          int32_t const ITC = itc[j];


          // write compressed
          if (type == 0  & ITC == 0) {
            const double val_d = Rcpp::as<Rcpp::NumericVector>(dat[j])[i];
            chnk[iter] = 253;

            ++iter;

            for (int8_t itr = iter; itr < 8; ++itr) {
              chnk[itr] = 0;
            }

            std::memcpy(&chunk, chnk, sizeof(double));
            writebin(chunk, sav, swapit);
            writebin(val_d, sav, swapit);

            // reset chnk
            for (int8_t itr = 0; itr < 8; ++itr) {
              chnk[itr] = 0;
            }

            iter = 0;
          }

          if (type == 0 & ITC == 1) {
            val_i =  Rcpp::as<Rcpp::IntegerVector>(dat[j])[i];
            val_b = val_i + 100;
            chnk[iter] = val_b;

            ++iter;
          }

          // write chunk of eight and clear chnk
          if (iter == 8) {

            std::memcpy(&chunk, chnk, sizeof(double));
            writebin(chunk, sav, swapit);

            // reset chnk
            for (int8_t itr = 0; itr < 8; ++itr) {
              chnk[itr] = 0;
            }

            iter = 0;
          }

          // write end of file
          if ((i == n-1) & (j == k -1)) {
            val_b = 252;
            chnk[iter] = val_b;

            std::memcpy(&chunk, chnk, sizeof(double));

            writebin(chunk, sav, swapit);
          }


        }
      }
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

            CharacterVector cv_s = NA_STRING;
            cv_s = as<CharacterVector>(dat[j])[i];

            string val_s = "";

            if (cv_s[0] != NA_STRING)
              val_s = as<string>(cv_s);

            writestr(val_s, type, sav);
            break;
          }

          }
        }
      }
    }

    sav.close();
  }

}
