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
#include "write_data.h"
#include "write_sav_compress.h"

//' writes the binary SPSS file
//'
//' @param filePath The full systempath to the dta file you want to import.
//' @param dat the data frame
//' @param compress the file
//' @param debug print debug information
//' @param is_zsav write zsav
//' @import Rcpp
//' @export
// [[Rcpp::export]]
void writesav(const char * filePath, Rcpp::DataFrame dat, uint8_t compress,
              bool debug, bool is_zsav)
{
  int32_t kk = dat.size(), k = 0;
  int64_t n = dat.nrows();


  std::fstream sav (filePath, std::ios::out | std::ios::binary);
  if (sav.is_open())
  {

    bool swapit = 0;

    int32_t rtype = 0, subtyp = 0, size = 0, count = 0;
    std::string empty = "";

    info_t info;

    info.vtyp = dat.attr("vtyp");
    info.cc = dat.attr("cc");
    info.itc = dat.attr("itc");
    info.vartypes = dat.attr("vartypes");
    info.vartyp = dat.attr("vartyp");

    info.nvarnames = dat.attr("nvarnames");
    info.label = dat.attr("label");

    info.haslabel = dat.attr("haslabel");
    info.labtab = dat.attr("labtab");
    info.disppar = dat.attr("disppar");

    std::string timestamp = Rcpp::as<std::string>(dat.attr("timestamp"));
    std::string datestamp = Rcpp::as<std::string>(dat.attr("datestamp"));
    std::string longvarname = Rcpp::as<std::string>(dat.attr("longvarnames"));

    // write correct k for string variables with nchar() > 8
    if (info.nvarnames.size() > kk)
      k = info.nvarnames.size();
    else
      k = kk;

    // Rprintf("k is: %d - kk is: %d\n", k, kk);

    std::string spss = "$FL2@(#)";
    if (is_zsav)
      spss = "$FL3@(#)";
    writestr(spss, spss.size(), sav);

    std::string datalabel = "readspss 0.13";
    writestr(datalabel, 56, sav);

    int32_t arch = 2;
    writebin(arch, sav, swapit);

    // int32_t kk = -1;
    writebin(k, sav, swapit);

    int32_t cflag=0, cwvariables = 0;

    if (compress)
      cflag = 1;
    if (is_zsav)
      cflag = 2;

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
    for (int i = 0; i < info.vartypes.size(); ++i) {
      rtype = 2;
      writebin(rtype, sav, swapit);

      int32_t isdate = info.vartyp[i];
      int32_t subtyp = info.vartypes[i];
      writebin(subtyp, sav, swapit);

      int32_t vlflag = 0;

      if (k == info.label.size())
        vlflag = 1;

      writebin(vlflag, sav, swapit);    // Label flag

      int32_t nmiss = 0;
      writebin(nmiss, sav, swapit);

      uint8_t unk41 = 0, unk42 = 0, unk43 = 0, unk44 = 0;
      // numeric
      if (subtyp == 0) {
        // factor
        if (isdate == -1) {
          unk41 = 0; // digits print format
          unk42 = 8; // field width
          unk43 = 5; // column format
          unk44 = 0; // not used?
        }
        // digit value
        if (isdate == 0) {
          unk41 = 2; // digits print format
          unk42 = 8; // field width
          unk43 = 5; // column format
          unk44 = 0; // not used?
        }
        // date
        if (isdate == 20 || isdate == 22) {
          unk41 = 0;
          unk42 = 10;
          unk43 = 39;
          unk44 = 0;
        }
      } else if (subtyp > 0) {
        // character
        unk41 = 0;
        unk42 = subtyp;
        unk43 = 1;
        unk44 = 0;
      } else if (subtyp == -1)  {
        unk41 = 1;
        unk42 = 29;
        unk43 = 1;
        unk44 = 0;
      }
      writebin(unk41, sav, 0);
      writebin(unk42, sav, 0);
      writebin(unk43, sav, 0);
      writebin(unk44, sav, 0);

      uint8_t unk51 = 0, unk52 = 0, unk53 = 0, unk54 = 0;
      // numeric
      if (subtyp == 0) {
        // factor value
        if (isdate == -1) {
          unk51 = 0; // digits format
          unk52 = 8; // field width
          unk53 = 5; // column format
          unk54 = 0; // not used?
        }
        // digit value
        if (isdate == 0) {
          unk51 = 2; // digits format
          unk52 = 8; // field width
          unk53 = 5; // column format
          unk54 = 0; // not used?
        }
        // date
        if (isdate == 20 || isdate == 22) {
          unk51 = 0;
          unk52 = 10;
          unk53 = 39;
          unk54 = 0;
        }
      } else if (subtyp > 0) {
        // character
        unk51 = 0;
        unk52 = subtyp;
        unk53 = 1;
        unk54 = 0;
      } else if (subtyp == -1)  {
        unk41 = 1;
        unk42 = 29;
        unk43 = 1;
        unk44 = 0;
      }
      writebin(unk51, sav, 0);
      writebin(unk52, sav, 0);
      writebin(unk53, sav, 0);
      writebin(unk54, sav, 0);

      std::string nvarname = Rcpp::as<std::string>(info.nvarnames[i]);
      writestr(nvarname, 8, sav);

      if (vlflag == 1) {

        std::string lab = Rcpp::as<std::string>(info.label[i]);

        int32_t origlen = 0;
        origlen = lab.size();
        origlen = ceil((double)origlen/4) * 4;

        writebin(origlen, sav, swapit);
        writestr(lab, origlen, sav);
      }

    }

    if(!Rf_isNull(info.haslabel))
    {

      // rtype 3 ---------------------------------------------------------------

      int32_t nolabels = info.haslabel.size();

      for (int i = 0; i < nolabels; ++i) {

        rtype = 3;
        writebin(rtype, sav, swapit);

        Rcpp::IntegerVector code = info.labtab(i);

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

        lab_id = info.haslabel[i];

        writebin(lab_id, sav, swapit);
      }

    }



    // rtype 7 -----------------------------------------------------------------

    // beign disppar
    rtype = 7;
    writebin(rtype, sav, swapit);

    subtyp = 11;
    writebin(subtyp, sav, swapit);

    size = 4;
    writebin(size, sav, swapit);

    count = info.disppar.size();
    writebin(count, sav, swapit);

    for (auto i = 0; i < count; ++i) {
      int32_t measure = info.disppar[i];
      writebin(measure, sav, swapit);
    }
    // end disppar

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

    if (is_zsav) {

      // write to temporary file
      // in this logic outfile = sav and sav = zsav
      const std::string tempstr = ".readspss_zsa_tmp_file";
      Rcpp::Rcout << tempstr.c_str() << std::endl;
      std::fstream tmp (tempstr, std::ios::out | std::ios::binary);
      if (!tmp.is_open()) Rcpp::stop("tmp not open");
      write_data(dat, cflag, n, kk, &info, tmp, swapit);
      tmp.close();

      // write zsav body
      write_sav_compress(sav, tempstr, swapit, debug);

      // // remove tempfile
      // std::remove(tempstr.c_str());

    } else {
      // write data part
      write_data(dat, cflag, n, kk, &info, sav, swapit);
    }

    sav.close();
  }

}
