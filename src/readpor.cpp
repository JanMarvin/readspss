/*
 * Copyright (C) 2018 Jan Marvin Garbuszus
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
//' @param override override bool
//' @import Rcpp
//' @export
// [[Rcpp::export]]
List readpor(const char * filePath, const bool debug, std::string encStr,
             bool override)
{

  std::string input;
  std::string file;
  stringstream por;

  std::ifstream por_file(filePath, std::ios::in | std::ios::binary);
  if (por_file) {

    while (getline(por_file, input)) {
      // some EDCDIC file I found is shorter than 80 characters
      // and contains at least a single empty line. Not sure if
      // SPSS causes this or the file host
      while (input.size() < 81){
        input += " ";
    }
      file += input;
    }

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

    std::vector<std::string> ftime;
    std::vector<std::string> fdate;
    std::vector<std::string> varnames;
    std::vector<std::string> varrangnams;
    std::vector<std::string> vn;
    std::vector<std::string> varlabels;
    std::vector<std::string> labelsetnams;
    std::vector<std::string> weightvars;


    std::string unkstr (1, '\0');


    Rcpp::NumericVector fmt_print_write(6);

    int nvarnames = 0, nlabelsetnams = 0;

    // 10 x "ASCII SPSS PORT FILE" in different encodings or all in the same
    // EBCDIC, 7-bit ASCII, CDC 6-bit ASCII, 6-bit ASCII, Honeywell 6-bit
    // ASCII

    std::string spss (200, '\0');
    spss = readstring(spss, por);

    if (!override){
      if (!std::regex_search(spss, std::regex("ASCII SPSS PORT FILE"))) {
          stop("The file header indicates that it is not an SPSS por file. "
               "Use 'override = TRUE' to ignore this check.");
      }
    }

    // Controll characters
    por.seekg(61, std::ios::cur);

    // Reserved
    por.seekg(3, std::ios::cur);

    // Digits 0 - 9
    std::string digits (10, '\0');
    digits = readstring(digits, por);

    if (debug)
      Rcout << "digits: " << digits << std::endl;

    // Capitals
    std::string capitals (26, '\0');
    capitals = readstring(capitals, por);

    if (debug)
      Rcout << "capitals: " << capitals << std::endl;

    // lowercase
    std::string lower (26, '\0');
    lower = readstring(lower, por);

    if (debug)
      Rcout << "lower: " << lower << std::endl;

    // random
    std::string random (61, '\0');
    random = readstring(random, por);

    if (debug)
      Rcout << "random: " << random << std::endl;

    // Reserved
    std::string reserved (69, '\0');
    reserved = readstring(reserved, por);

    // tag
    std::string tag (8, '\0');
    tag = readstring(tag, por);

    if (debug)
      Rcout << "tag: " << tag << std::endl;

    if (debug)
      Rcout << "Pos: " << por.tellg() << std::endl;

    // end of header -----------------------------------------------------------


    // version and date record
    std::string vers (1, '\0');
    vers = readstring(vers, por);

    // filedate is yyyymmdd should be 8
    std::string filedatelen = string(1, '\0');
    filedatelen = readtostring(por);

    std::string slash = string(1, '\0');

    std::string filedate (std::stoi(filedatelen), '\0');
    filedate = readstring(filedate, por);
    fdate.push_back(filedate);

    // filetime is hhmmss should be 6
    std::string filetimelen = string(1, '\0');
    filetimelen = readtostring(por);

    std::string filetime (std::stoi(filetimelen), '\0');
    filetime = readstring(filetime, por);
    ftime.push_back(filetime);


    if (debug)
      Rcout << vers << " " << filedate << " " << filetime << std::endl;


    std::string varrec (1, '\0');

    // 1 : identification record
    varrec = readstring(varrec, por);

    // can be base-30 digit
    std::string prodlen;
    prodlen = readtostring(por);

    // strtoi as in R
    std::string prod ( b30int(prodlen), '\0');
    prod = readstring(prod, por);

    file_info.push_back(prod);


    if (debug)
      Rcout << prod << std::endl;

    // optional
    // 2 or 3 : author and extra record
    varrec = readstring(varrec, por);

    // optional
    // 2 : author
    if (varrec.compare("2") == 0) {

      // can be base-30 digit if 0 then read until next digit
      std::string authorlen;
      authorlen = readtostring(por);

      // author name
      std::string author ( b30int(authorlen), '\0');
      author = readstring(author, por);

      file_info.push_back(author);

      if (debug)
        Rcout << author << std::endl;

      varrec = readstring(varrec, por);
    }

    // 3 : extra record
    if (varrec.compare("3") == 0) {

      // can be base-30 digit if 0 then read until next digit
      std::string extralen;
      extralen = readtostring(por);

      // extra information
      std::string extra ( b30int(extralen), '\0');
      extra = readstring(extra, por);

      file_info.push_back(extra);

      if (debug)
        Rcout << extra << std::endl;

      varrec = readstring(varrec, por);
    }

    int vars = 0;

    while (1) {
      Rcpp::checkUserInterrupt();


      // 4 : variables record
      if (varrec.compare("4") == 0) {

        // number of vars
        std::string varsize;
        varsize = readtostring(por);

        vars = b30int(varsize);

        if (debug)
          Rprintf("varsize: %d\n", vars);

        varrec = readstring(varrec, por);
      }


      // 5 : precision record
      if (varrec.compare("5") == 0) {

        if (debug)
          Rcout << "--- 5 ---" << std::endl;

        std::string prec;
        prec = readtostring(por);
        prec = b30str(prec);

        varrec = readstring(varrec, por);

      }

      // 6 : weighting record
      if (varrec.compare("6") == 0) {

        if (debug)
          Rcout << "--- 6 ---" << std::endl;

        // single string
        std::string len;
        len = readtostring(por);

        std::string wvar( b30int(len), '\0');
        wvar = readstring(wvar, por);
        weightvars.push_back(wvar);

        varrec = readstring(varrec, por);

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
        vartypes.push_back( b30int(vartyp) );

        // varnamelen (1 - 8)
        std::string varnamelen;
        varnamelen = readtostring(por);

        std::string varname ( b30int(varnamelen), '\0' );
        varname = readstring(varname, por);

        // varname
        varnames.push_back(varname);
        ++nvarnames;

        double fmt_d = 0.0;

        /* Printformat */
        // 5 Format typ
        unkstr = readtostring(por);

        fmt_d = dnum(unkstr);
        fmt_print_write(0) = fmt_d;


        // 8 Format width:  1-40
        unkstr = readtostring(por);

        fmt_d = dnum(unkstr);
        fmt_print_write(1) = fmt_d;

        // 2 Number of decimalplaces: 1-40
        unkstr = readtostring(por);

        fmt_d = dnum(unkstr);
        fmt_print_write(2) = fmt_d;

        /* Writeformat */
        // 5
        unkstr = readtostring(por);

        fmt_d = dnum(unkstr);
        fmt_print_write(3) = fmt_d;

        // 8
        unkstr = readtostring(por);

        fmt_d = dnum(unkstr);
        fmt_print_write(4) = fmt_d;

        // 2
        unkstr = readtostring(por);

        fmt_d = dnum(unkstr);
        fmt_print_write(5) = fmt_d;


        fmt.push_back(fmt_print_write);

        // varrec
        varrec = readstring(varrec, por);


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
          std::string miss( b30int(misslen), '\0' );
          miss = readstring(miss, por);

          miss_val = miss;
        } else {
          // same name as char
          miss_val = b30str(misslen);
        }

        // create named char and push back
        Rcpp::CharacterVector missCV = miss_val;
        missCV.attr("names") = miss_nam;
        missings.push_back(missCV);

        varrec = readstring(varrec, por);

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
        res(0) = b30int(lowthrux);

        res.attr("names") = varname;


        loThruX.push_back(res);

        varrec = readstring(varrec, por);
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
        res(0) = b30int(xthruhigh);

        res.attr("names") = varname;


        xThruHi.push_back(res);

        varrec = readstring(varrec, por);

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

        varrangCV(0) = b30str(minval);
        varrangCV(1) = b30str(maxval);

        varrangnams.push_back(varname);

        varrange.push_back(varrangCV);
        varrange.attr("names") = varrangnams;

        varrec = readstring(varrec, por);

      }


      // variable label
      if (varrec.compare("C") == 0) {

        if (debug)
          Rcout << "--- C ---" << std::endl;

        std::string labellen;

        labellen = readtostring(por);

        std::string label( b30int(labellen), '\0');
        label = readstring(label, por);

        if (debug)
          Rcout << label << std::endl;

        varlabels.push_back(label);

        varrec = readstring(varrec, por);

      }

      // D : value labels
      if (varrec.compare("D") == 0) {

        if (debug)
          Rcout << "--- D ---" << std::endl;

        std::string unk1;
        unk1 = readtostring(por);

        int nolab = 0;
        nolab = b30int(unk1);

        // chained labelsets
        for (int i = 0; i < nolab; ++i) {

          std::string labelset;
          labelset = readtostring(por);

          std::string labelsetnam( b30int(labelset), '\0');
          labelsetnam = readstring(labelsetnam, por);

          if (debug)
            Rcout << labelsetnam << std::endl;

          labelsetnams.push_back(labelsetnam);
          ++nlabelsetnams;

        }

        std::string labelnum;
        labelnum = readtostring(por);

        if (debug)
          Rcout << labelnum << std::endl;

        int labnums = 0;
        labnums = b30int(labelnum);
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

            if (debug) {
              Rcout << "l & t: " << labval << " " << labtxtlen << std::endl;
            }

            std::string labtxt ( b30int(labtxtlen), '\0');
            labtxt = readstring(labtxt, por);

            double lab_d = 0.0;
            lab_d = dnum(labval);

            labvals[i] = std::to_string(lab_d);
            labtxts[i] = labtxt;
          }

          labvals.attr("names") = labtxts;

        } else {

          for (int i = 0; i < labnums; ++i) {
            std::string labval_len;
            labval_len = readtostring(por);

            std::string labval( b30int(labval_len), '\0');
            labval = readstring(labval, por);

            std::string labtxtlen;
            labtxtlen = readtostring(por);

            std::string labtxt( b30int(labtxtlen), '\0');
            labtxt = readstring(labtxt, por);

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

        varrec = readstring(varrec, por);

      }


      // documentation
      if (varrec.compare("E") == 0) {

        if (debug)
          Rcout << "--- E ---" << std::endl;

        std::string doclen;
        int doclen_i = 0;

        doclen = readtostring(por);
        doclen_i = b30int(doclen);


        for (int i = 0; i < doclen_i; ++i)
        {
          std::string linelen;
          linelen = readtostring(por);

          std::string docline( b30int(linelen), '\0');
          docline = readstring(docline, por);

          if (debug)
            Rcout << docline << std::endl;

          doc.push_back(docline);
        }

        varrec = readstring(varrec, por);

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

      if (debug)
        Rcout << "--- F ---" << std::endl;

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

          if (debug)
            Rprintf("type: %d\n", type);

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
              int val_s_len = b30int(val);

              std::string val_s (val_s_len, '\0');
              val_s = readstring(val_s, por);

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
            val_d = dnum(val);

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
            int val_s_len = b30int(val);

            std::string val_s (val_s_len, '\0');
            val_s = readstring(val_s, por);

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

    df.attr("filetime") = ftime;
    df.attr("filedate") = fdate;
    df.attr("label") = varlabels;
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
