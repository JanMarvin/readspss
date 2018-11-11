/*
 * Copyright (C) 2014-2015 Jan Marvin Garbuszus
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
#include <regex>

using namespace Rcpp;
using namespace std;

#include "spss.h"

//' Reads the binary SPSS file
//'
//' @param filePath The full systempath to the dta file you want to import.
//' @param debug print debug information
//' @param encStr encoding string
//' @import Rcpp
//' @export
// [[Rcpp::export]]
List readpor(const char * filePath, const bool debug, std::string encStr)
{

  std::string input;
  std::string file;
  stringstream por;

  std::ifstream por_file(filePath, std::ios::in | std::ios::binary);
  if (por_file) {

    while (getline(por_file, input))
      file += input;

  } else {
    stop ("No file was read.");
  }
  por_file.close();

  // remove all newline characters \r and \n
  file.erase( std::remove(file.begin(), file.end(), '\r'), file.end() );
  file.erase( std::remove(file.begin(), file.end(), '\n'), file.end() );

  por << file;


  if (por) {


    Rcpp::List missings;
    Rcpp::List varrange;
    Rcpp::List labtab;
    Rcpp::List file_info;
    Rcpp::List loThruX;
    Rcpp::List xThruHi;
    Rcpp::List fmt;
    Rcpp::List doc;

    std::vector<int> vartypes;

    std::vector<std::string> varnames;
    std::vector<std::string> varrangnams;
    std::vector<std::string> vn;
    std::vector<std::string> varlabels;
    std::vector<std::string> labelsetnams;
    std::vector<std::string> weightvars;


    std::string unkstr (1, '\0');


    Rcpp::NumericVector fmt_print_write(6);

    int nvarnames = 0, nlabelsetnams = 0;

    // 5 x SPSS PORT FILE
    // EBCDIC, 7-bit ASCII, CDC 6-bit ASCII, 6-bit ASCII, Honeywell 6-bit
    // ASCII
    std::string spss (20, '\0');
    std::string espss1 (20, '\0');
    std::string spss20 (20, '\0');
    std::string spss40 (40, '\0');

    // 1
    spss = readstring(spss, por, spss.size());

    std::string ebcdic = "IBM037"; // EBCDIC
    std::string cdc = "ASCII";

    if (spss.compare("ASCII SPSS PORT FILE") != 0) {

      espss1 = Riconv(spss, ebcdic);

      if (espss1.compare("ASCII SPSS PORT FILE") != 0)
        stop("header indicates file is no spss por file");
    }

    spss20 = readstring(spss20, por, spss20.size());

    // 2
    spss40 = readstring(spss40, por, spss40.size());

    // 3
    spss40 = readstring(spss40, por, spss40.size());

    // 4
    spss40 = readstring(spss40, por, spss40.size());

    // 5
    spss40 = readstring(spss40, por, spss40.size());


    // Controll characters
    por.seekg(61, std::ios::cur);

    // Reserved
    por.seekg(3, std::ios::cur);

    // Digits 0 - 9
    std::string digits (10, '\0');
    digits = readstring(digits, por, digits.size());

    if (debug)
      Rcout << "digits: " << digits << std::endl;

    // Capitals
    std::string capitals (26, '\0');
    capitals = readstring(capitals, por, capitals.size());

    if (debug)
      Rcout << "capitals: " << capitals << std::endl;

    // lowercase
    std::string lower (26, '\0');
    lower = readstring(lower, por, lower.size());

    if (debug)
      Rcout << "lower: " << lower << std::endl;

    // random
    std::string random (61, '\0');
    random = readstring(random, por, random.size());

    if (debug)
      Rcout << "random: " << random << std::endl;

    // Reserved
    std::string reserved (69, '\0');
    reserved = readstring(reserved, por, reserved.size());

    // tag
    std::string tag (8, '\0');
    tag = readstring(tag, por, tag.size());

    if (debug)
      Rcout << "tag: " << tag << std::endl;

    if (debug)
      Rcout << "Pos: " << por.tellg() << std::endl;

    // end of header -----------------------------------------------------------


    // version and date record
    std::string vers (1, '\0');
    vers = readstring(vers, por, vers.size());

    // filedate is yyyymmdd should be 8
    std::string filedatelen = string(1, '\0');
    filedatelen = readstring(filedatelen, por, filedatelen.size());

    std::string slash = string(1, '\0');
    readstring(slash, por, slash.size());

    std::string filedate (std::stoi(filedatelen), '\0');
    filedate = readstring(filedate, por, filedate.size());

    // filetime is hhmmss should be 6
    std::string filetimelen = string(1, '\0');
    filetimelen = readstring(filetimelen, por, filetimelen.size());

    readstring(slash, por, slash.size());

    std::string filetime (std::stoi(filetimelen), '\0');
    filetime = readstring(filetime, por, filetime.size());


    if (debug)
      Rcout << vers << " " << filedate << " " << filetime << std::endl;


    std::string varrec (1, '\0');

    // 1 : identification record
    varrec = readstring(varrec, por, varrec.size());

    // can be base-30 digit
    std::string prodlen;
    prodlen = readtostring(por);

    // strtoi as in R
    std::string prod (std::stol(prodlen, NULL, 30), '\0');
    prod = readstring(prod, por, prod.size());

    file_info.push_back(prod);


    if (debug)
      Rcout << prod << std::endl;

    // optional
    // 2 or 3 : author and extra record
    varrec = readstring(varrec, por, varrec.size());

    // optional
    // 2 : author
    if (varrec.compare("2") == 0) {

      // can be base-30 digit if 0 then read until next digit
      std::string authorlen;
      authorlen = readtostring(por);

      // author name
      std::string author (std::stol(authorlen, NULL, 30), '\0');
      author = readstring(author, por, author.size());

      file_info.push_back(author);

      if (debug)
        Rcout << author << std::endl;

      varrec = readstring(varrec, por, varrec.size());
    }

    // 3 : extra record
    if (varrec.compare("3") == 0) {

      // can be base-30 digit if 0 then read until next digit
      std::string extralen;
      extralen = readtostring(por);

      // extra information
      std::string extra (std::stol(extralen, NULL, 30), '\0');
      extra = readstring(extra, por, extra.size());

      file_info.push_back(extra);

      if (debug)
        Rcout << extra << std::endl;

      varrec = readstring(varrec, por, varrec.size());
    }

    int vars = 0;

    while (1) {
      Rcpp::checkUserInterrupt();


      // 4 : variables record
      if (varrec.compare("4") == 0) {

        // number of vars
        std::string varsize;
        varsize = readtostring(por);

        vars = std::strtol(varsize.c_str(), NULL, 30);

        if (debug)
          Rprintf("varsize: %d\n", vars);

        varrec = readstring(varrec, por, varrec.size());
      }


      // 5 : precision record
      if (varrec.compare("5") == 0) {

        if (debug)
          Rcout << "--- 5 ---" << std::endl;

        std::string prec;
        prec = std::to_string(std::strtol(readtostring(por).c_str(), NULL, 30));

        varrec = readstring(varrec, por, varrec.size());

      }

      // 6 : weighting record
      if (varrec.compare("6") == 0) {

        if (debug)
          Rcout << "--- 6 ---" << std::endl;

        // single string
        std::string len;
        len = readtostring(por);

        std::string wvar(std::strtol(len.c_str(), NULL, 30), '\0');
        wvar = readstring(wvar, por, wvar.size());
        weightvars.push_back(wvar);

        varrec = readstring(varrec, por, varrec.size());

      }

      // 7 : variable records
      if (varrec.compare("7") == 0)
      {

        if (debug)
          Rcout << "--- 7 ---" << std::endl;

        // 0 or 1-255
        std::string vartyp;
        vartyp = readtostring(por);

        // vartype
        vartypes.push_back(std::strtol(vartyp.c_str(), NULL, 30));

        // varnamelen (1 - 8)
        std::string varnamelen;
        varnamelen = readtostring(por);

        std::string varname (std::atoi(varnamelen.c_str()), '\0');
        varname = readstring(varname, por, varname.size());

        // varname
        varnames.push_back(varname);
        ++nvarnames;

        double fmt_d = 0.0;
        int mv = 0;

        /* Printformat */
        // 5 Format typ
        unkstr = readtostring(por);

        if (dnum(&unkstr[0], fmt_d, &mv) == 1)
          fmt_print_write(0) = fmt_d;


        // 8 Format width:  1-40
        unkstr = readtostring(por);

        if (dnum(&unkstr[0], fmt_d, &mv) == 1)
          fmt_print_write(1) = fmt_d;

        // 2 Number of decimalplaces: 1-40
        unkstr = readtostring(por);

        if (dnum(&unkstr[0], fmt_d, &mv) == 1)
          fmt_print_write(2) = fmt_d;

        /* Writeformat */
        // 5
        unkstr = readtostring(por);

        if (dnum(&unkstr[0], fmt_d, &mv) == 1)
          fmt_print_write(3) = fmt_d;

        // 8
        unkstr = readtostring(por);

        if (dnum(&unkstr[0], fmt_d, &mv) == 1)
          fmt_print_write(4) = fmt_d;

        // 2
        unkstr = readtostring(por);

        if (dnum(&unkstr[0], fmt_d, &mv) == 1)
          fmt_print_write(5) = fmt_d;


        fmt.push_back(fmt_print_write);

        // varrec
        varrec = readstring(varrec, por, varrec.size());


        if (debug) {
          Rcout << varname << std::endl;
          Rcout << varnamelen << std::endl;
        }
      }


      // missing values
      if (varrec.compare("8") == 0) {

        if (debug)
          Rcout << "--- 8 ---" << std::endl;
        int vartyp = 0;

        std::string misslen;
        misslen = readtostring(por);

        // char for > 0 otherwise its integer
        ptrdiff_t pos = 0;
        pos = distance(varnames.begin(), find(varnames.begin(),
                                      varnames.end(),
                                      varnames[nvarnames-1]));

        vartyp = vartypes[pos];
        std::string miss_nam = varnames[pos];
        std::string miss_val;

        if (vartyp > 0) {
          std::string miss(std::strtol(misslen.c_str(), NULL, 30), '\0');
          miss = readstring(miss, por, miss.size());

          miss_val = miss;
        } else {
          // same name as char
          miss_val = std::to_string(std::strtol(misslen.c_str(), NULL, 30));
        }

        // create named char and push back
        Rcpp::CharacterVector missCV = miss_val;
        missCV.attr("names") = miss_nam;
        missings.push_back(missCV);

        varrec = readstring(varrec, por, varrec.size());

      }


      // low thru x
      if (varrec.compare("9") == 0) {

        std::string lowthrux;
        lowthrux = readtostring(por);

        std::string varname;
        ptrdiff_t pos = 0;

        pos = distance(varnames.begin(), find(varnames.begin(),
                                      varnames.end(),
                                      varnames[nvarnames-1]));

        varname = varnames[pos];

        Rcpp::NumericVector res(1);
        res(0) = std::strtol(lowthrux.c_str(), NULL, 30);

        res.attr("names") = varname;


        loThruX.push_back(res);

        varrec = readstring(varrec, por, varrec.size());
      }


      // x thru high
      if (varrec.compare("A") == 0) {

        std::string xthruhigh;
        xthruhigh = readtostring(por);

        std::string varname;
        ptrdiff_t pos = 0;

        pos = distance(varnames.begin(), find(varnames.begin(),
                                      varnames.end(),
                                      varnames[nvarnames-1]));

        varname = varnames[pos];

        Rcpp::NumericVector res(1);
        res(0) = std::strtol(xthruhigh.c_str(), NULL, 30);

        res.attr("names") = varname;


        xThruHi.push_back(res);

        varrec = readstring(varrec, por, varrec.size());

      }


      // range of min and max val
      if (varrec.compare("B") == 0) {

        if (debug)
          Rcout << "--- B ---" << std::endl;
        std::string varname;

        ptrdiff_t pos = 0;
        pos = distance(varnames.begin(), find(varnames.begin(),
                                      varnames.end(),
                                      varnames[nvarnames-1]));

        varname = varnames[pos];

        std::string minval;
        std::string maxval;

        Rcpp::CharacterVector varrangCV(2);

        minval = readtostring(por); // min value
        maxval = readtostring(por); // max value

        varrangCV(0) = std::to_string(std::strtol(minval.c_str(), NULL, 30));
        varrangCV(1) = std::to_string(std::strtol(maxval.c_str(), NULL, 30));

        varrangnams.push_back(varname);

        varrange.push_back(varrangCV);
        varrange.attr("names") = varrangnams;

        varrec = readstring(varrec, por, varrec.size());

      }


      // variable label
      if (varrec.compare("C") == 0) {

        if (debug)
          Rcout << "--- C ---" << std::endl;

        std::string labellen;

        labellen = readtostring(por);

        std::string label(std::strtol(labellen.c_str(), NULL, 30), '\0');
        label = readstring(label, por, label.size());

        if (debug)
          Rcout << label << std::endl;

        varlabels.push_back(label);

        varrec = readstring(varrec, por, varrec.size());

      }

      // D : value labels
      if (varrec.compare("D") == 0) {

        if (debug)
          Rcout << "--- D ---" << std::endl;

        std::string unk1;
        unk1 = readtostring(por);

        int nolab = 0;
        nolab = std::strtol(unk1.c_str(), NULL, 30);

        // chained labelsets
        for (int i = 0; i < nolab; ++i) {

          std::string labelset;
          labelset = readtostring(por);

          std::string labelsetnam(std::strtol(labelset.c_str(),
                                              NULL, 30), '\0');
          labelsetnam = readstring(labelsetnam, por, labelsetnam.size());

          labelsetnams.push_back(labelsetnam);
          ++nlabelsetnams;

        }

        std::string labelnum;
        labelnum = readtostring(por);

        int labnums = 0;
        labnums = std::strtol(labelnum.c_str(), NULL, 30);
        Rcpp::CharacterVector labvals(labnums);
        Rcpp::CharacterVector labtxts(labnums);

        ptrdiff_t pos = distance(varnames.begin(),
                                 find(varnames.begin(),
                                      varnames.end(),
                                      labelsetnams[nlabelsetnams-1]));

        int vartyp = 0;
        vartyp = vartypes[pos];

        if (vartyp == 0) {

          for (int i = 0; i < labnums; ++i) {

            std::string labval;
            std::string labtxtlen;

            labval = readtostring(por);
            labtxtlen = readtostring(por);

            std::string labtxt (std::strtol(labtxtlen.c_str(), NULL, 30), '\0');
            labtxt = readstring(labtxt, por, labtxt.size());

            labvals[i] = std::to_string(std::strtol(labval.c_str(), NULL, 30));
            labtxts[i] = labtxt;
          }

          labvals.attr("names") = labtxts;

        } else {

          for (int i = 0; i < labnums; ++i) {
            std::string labval_len;
            labval_len = readtostring(por);

            std::string labval(std::strtol(labval_len.c_str(), NULL, 30), '\0');
            labval = readstring(labval, por, labval.size());

            std::string labtxtlen;
            labtxtlen = readtostring(por);

            std::string labtxt (std::strtol(labtxtlen.c_str(), NULL, 30), '\0');
            labtxt = readstring(labtxt, por, labtxt.size());

            labvals[i] = labval;
            labtxts[i] = labtxt;

          }

          labvals.attr("names") = labtxts;
        }

        if (debug) {
          Rcout << labtxts << std::endl;
          Rcout << labvals <<std::endl;
        }

        // push it back nolab times so that it matches the labelsetnams
        for (int i = 0; i < nolab; ++i)
          labtab.push_back(labvals);

        labtab.attr("names") = labelsetnams;

        varrec = readstring(varrec, por, varrec.size());

      }


      // documentation
      if (varrec.compare("E") == 0) {

        if (debug)
          Rcout << "--- E ---" << std::endl;

        std::string doclen;
        int doclen_i = 0;

        doclen = readtostring(por);
        doclen_i = strtol(doclen.c_str(), NULL, 30);


        for (int i = 0; i < doclen_i; ++i)
        {
          std::string linelen;
          linelen = readtostring(por);

          std::string docline(std::strtol(linelen.c_str(), NULL, 30), '\0');
          docline = readstring(docline, por, docline.size());

          if (debug)
            Rcout << docline << std::endl;

          doc.push_back(docline);
        }

        varrec = readstring(varrec, por, varrec.size());

      }


      // data part reached
      if ((varrec.compare("F") == 0) || (varrec.compare("Z") == 0))
        break;
    }


    if (debug)
      Rcout << "varrec " << varrec << std::endl;

    int n = 0;
    int nvars = 0;
    nvars = varnames.size();

    bool eof = false;

    // Create Rcpp::List
    Rcpp::List df(nvars);


    // Data part found. Starts with F ends with Z...
    if ( varrec.compare("F") == 0) {

      size_t data_begin = por.tellg();

      // dry run until end of file is reached ----------------------------------
      for (int kk = 0; kk < R_PosInf; ++kk) {
        for (int ii = 0; ii < vars; ++ii) {

          std::string val;
          val = readtostring(por);

          if (debug)
            Rcout << val << std::endl;

          // check that eof is really reached and not only a string "Z"
          // if (por.peek() == EOF)
          eof = val.find_first_not_of("Z") == string::npos;

          if (eof) {
            if (debug)
              Rcout << "End of file found. n is " << n << std::endl;
            break;
          }

          int const type = vartypes[ii];
          switch(type)
          {
          case 0:
          {
            // if (debug)
            //   Rcout << "numeric do nothing" << std::endl;
            break;
          }

          default:
          {
            int val_s_len = std::strtol(val.c_str(), NULL, 30);

            std::string val_s (val_s_len, '\0');
            val_s = readstring(val_s, por, val_s.size());

            break;
          }
          }

        }

        if (eof)
          break;

        ++n;

      }


      if (debug)
        Rcout << "Dry run finished. n is known import beginns" << std::endl;

      // back to the data_begin part
      por.seekg(data_begin);

    }


    // 1. Create data frame --------------------------------------------------
    for (int32_t i=0; i<nvars; ++i)
    {
      int const type = vartypes[i];
      switch(type)
      {
      case 0:
        SET_VECTOR_ELT(df, i, Rcpp::NumericVector(Rcpp::no_init(n)));
        break;

      default:
        SET_VECTOR_ELT(df, i, Rcpp::CharacterVector(Rcpp::no_init(n)));
      break;
      }
    }


    if ( varrec.compare("F") == 0) {
      // 2 . fill list with data -----------------------------------------------
      for (int kk = 0; kk < n; ++kk) {
        for (int ii = 0; ii < vars; ++ii) {

          std::string val;
          val = readtostring(por);

          int const type = vartypes[ii];

          switch(type)
          {
          case 0:
          {
            double val_d = 0.0;
            int    mv = 0;

            std::vector<char> cstr(val.begin(), val.end());
            cstr.push_back('\0');

            // TDA function
            if (dnum(&cstr[0], val_d, &mv) != 1)
              stop("reading numeric failed");

            if (debug) {
              Rcout << varnames[ii] << std::endl;

              Rcout << val << std::endl;
              Rprintf("%f\n", val_d);
            }


            REAL(VECTOR_ELT(df,ii))[kk] = val_d;

            break;
          }

          default:
          {
            int val_s_len = std::strtol(val.c_str(), NULL, 30);

            std::string val_s (val_s_len, '\0');
            val_s = readstring(val_s, por, val_s.size());

            Rcpp::as<Rcpp::CharacterVector>(df[ii])[kk] = val_s;

            break;
          }
          }

        }

      }
    }


    // 3. Create a data.frame
    R_xlen_t nrows = Rf_length(df[0]);
    df.attr("row.names") = IntegerVector::create(NA_INTEGER, nrows);
    df.attr("names") = varnames;
    df.attr("class") = "data.frame";

    df.attr("labels") = varlabels;
    df.attr("missings") = missings;
    df.attr("labtab") = labtab;
    df.attr("vartypes") = vartypes;
    df.attr("varrange") = varrange;
    df.attr("weightvars") = weightvars;

    df.attr("fmt") = fmt;
    df.attr("file_info") = file_info;
    df.attr("lothrux") = loThruX;
    df.attr("xthruhi") = xThruHi;
    df.attr("doc") = doc;

    return(df);


  } else {
    return (-1);
  }
}
