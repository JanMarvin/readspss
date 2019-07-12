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

    Rcpp::IntegerVector vtyp = dat.attr("vtyp");
    Rcpp::IntegerVector cc = dat.attr("cc");
    Rcpp::IntegerVector itc = dat.attr("itc");
    Rcpp::IntegerVector vartypes = dat.attr("vartypes");
    Rcpp::IntegerVector vartyp = dat.attr("vartyp");

    Rcpp::CharacterVector nvarnames = dat.attr("nvarnames");
    Rcpp::CharacterVector label = dat.attr("label");

    Rcpp::IntegerVector haslabel = dat.attr("haslabel");
    Rcpp::List labtab = dat.attr("labtab");

    std::string timestamp = Rcpp::as<std::string>(dat.attr("timestamp"));
    std::string datestamp = Rcpp::as<std::string>(dat.attr("datestamp"));
    std::string longvarname = Rcpp::as<std::string>(dat.attr("longvarnames"));

    // write correct k for string variables with nchar() > 8
    if (nvarnames.size() > kk)
      k = nvarnames.size();
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
    for (int i = 0; i < vartypes.size(); ++i) {
      rtype = 2;
      writebin(rtype, sav, swapit);

      int32_t isdate = vartyp[i];
      int32_t subtyp = vartypes[i];
      writebin(subtyp, sav, swapit);

      int32_t vlflag = 0;

      if (k == label.size())
        vlflag = 1;

      writebin(vlflag, sav, swapit);    // Label flag

      int32_t nmiss = 0;
      writebin(nmiss, sav, swapit);

      int_chars c;

      int32_t var4;
      char tmp1[4] = "";
      if (subtyp == 0) {
        tmp1[0] = 2;
        tmp1[1] = 8;
        tmp1[2] = isdate;
        tmp1[3] = 0;
      } else if (subtyp > 0) {
        tmp1[0] = 0;
        tmp1[1] = subtyp;
        tmp1[2] = 1;
        tmp1[3] = 0;
      } else if (subtyp == -1) {
        tmp1[0] = 1;
        tmp1[1] = 29;
        tmp1[2] = 1;
        tmp1[3] = 0;
      }

      c.b[0] = tmp1[0];
      c.b[1] = tmp1[1];
      c.b[2] = tmp1[2];
      c.b[3] = tmp1[3];

      var4 = c.a;
      writebin(var4, sav, 0);

      int32_t var5;
      char tmp2[4] = "";
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
        tmp2[0] = 1;
        tmp2[1] = 29;
        tmp2[2] = 1;
        tmp2[3] = 0;
      }

      c.b[0] = tmp2[0];
      c.b[1] = tmp2[1];
      c.b[2] = tmp2[2];
      c.b[3] = tmp2[3];

      var5 = c.a;
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

    if (is_zsav) {

      // write to temporary file
      // in this logic outfile = sav and sav = zsav
      const std::string tempstr = std::tmpnam(nullptr);
      std::fstream tmp (tempstr, std::ios::out | std::ios::binary);

      // write data part to tmp file
      write_data(dat, cflag,n, kk, vtyp, itc, cc, tmp, swapit);

      tmp.close();

      // write zsav body
      write_sav_compress(sav, tempstr, swapit, debug);

      // remove tempfile
      std::remove(tempstr.c_str());

    } else {
      // write data part
      write_data(dat, cflag,n, kk, vtyp, itc, cc, sav, swapit);
    }

    sav.close();
  }

}
