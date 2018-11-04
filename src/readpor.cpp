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

  // std::regex e("([^\r\n]|^)\r\n([^\r\n]|$)");
  // std::cout << std::regex_replace(file, e, "$1$2") << std::endl;

  // remove all newline characters \r and \n
  file.erase( std::remove(file.begin(), file.end(), '\r'), file.end() );
  file.erase( std::remove(file.begin(), file.end(), '\n'), file.end() );

  por << file;


  if (por) {

    // int32_t n = 0;
    // int32_t k = 0;<

    // 5 x SPSS PORT FILE
    // EBCDIC, 7-bit ASCII, CDC 6-bit ASCII, 6-bit ASCII, Honeywell 6-bit
    // ASCII
    std::string spss (40, '\0');

    // 1
    spss = readstring(spss, por, spss.size());
    // Rcout << spss << std::endl;

    // 2
    spss = readstring(spss, por, spss.size());
    // Rcout << spss << std::endl;

    // 3
    spss = readstring(spss, por, spss.size());
    // Rcout << spss << std::endl;

    // 4
    spss = readstring(spss, por, spss.size());
    // Rcout << spss << std::endl;

    // 5
    spss = readstring(spss, por, spss.size());
    // Rcout << spss << std::endl;

    // Rcpp::stop("Debug!");
    if (debug)
      Rcout << "Pos: " << por.tellg() << std::endl;


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
    // Rprintf("lower: %s \n", lower.c_str());

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

    // Rcout << prodlen << std::endl;

    // readstring(slash, por, slash.size());

    // strtoi as in R
    std::string prod (std::stol(prodlen, NULL, 30), '\0');
    prod = readstring(prod, por, prod.size());


    if (debug)
      Rcout << prod << std::endl;

    // optional
    // 2 or 3 : author and extra record
    varrec = readstring(varrec, por, varrec.size());

    // optional
    // 2 : author
    if (varrec.compare("2") == 0) {
      stop("unhandled case 2");
    }

    // 3 : extra record
    if (varrec.compare("3") == 0) {

      // can be base-30 digit if 0 then read until next digit
      std::string extralen (1, '\0');
      extralen = readstring(extralen, por, extralen.size());

      readstring(slash, por, slash.size());

      // extra information
      std::string extra (std::stol(extralen, NULL, 30), '\0');
      extra = readstring(extra, por, extra.size());


      if (debug)
        Rcout << extra << std::endl;

      varrec = readstring(varrec, por, varrec.size());
    }

    int vars = 0;


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

      std::string prec (1, '\0');
      prec = readstring(prec, por, prec.size());

      int precs = 0;
      precs = std::strtol(prec.c_str(), NULL, 30);

      readstring(slash, por, slash.size());
      varrec = readstring(varrec, por, varrec.size());

    }

    // 6 : weighting record
    if (varrec.compare("6") == 0) {
      // single string
      stop("unhandled case 6");
    }


    std::vector<int> vartypes;
    std::vector<std::string> varnames;
    std::vector<std::string> varlabels;
    std::vector<std::string> missings;
    std::string unkstr (1, '\0');


    // 7 : variable records
    while (varrec.compare("7") == 0)
    {

      // 0 or 1-255
      std::string vartyp;
      vartyp = readtostring(por);

      // vartype
      vartypes.push_back(std::strtol(vartyp.c_str(), NULL, 30));


      // varnamelen (1 - 8)
      std::string varnamelen (1, '\0');
      varnamelen = readstring(varnamelen, por, varnamelen.size());
      readstring(slash, por, slash.size());

      std::string varname (std::atoi(varnamelen.c_str()), '\0');
      varname = readstring(varname, por, varname.size());

      // varname
      varnames.push_back(varname);

      /* Printformat */
      // 5 Format typ
      unkstr = readtostring(por);

      // 8 Format width:  1-40
      unkstr = readtostring(por);

      // 2 Number of decimalplaces: 1-40
      unkstr = readtostring(por);

      /* Writeformat */
      // 5
      unkstr = readtostring(por);

      // 8
      unkstr = readtostring(por);

      // 2
      unkstr = readtostring(por);

      // varrec
      varrec = readstring(varrec, por, varrec.size());


      if (debug) {
        Rcout << varname << std::endl;
        Rcout << varnamelen << std::endl;
      }

      // missing values
      while (varrec.compare("8") == 0) {

        std::string misslen;

        misslen = readtostring(por);

        std::string miss(std::strtol(misslen.c_str(), NULL, 30), '\0');
        miss = readstring(miss, por, miss.size());

        missings.push_back(miss);

        varrec = readstring(varrec, por, varrec.size());

        warning("Belongs to %s", varname);

      }


      // low thru x
      while (varrec.compare("9") == 0) {
        stop("unhandled case 9");
      }


      // x thru high
      while (varrec.compare("A") == 0) {
        stop("unhandled case A");
      }

      // variable label
      while (varrec.compare("C") == 0) {

        std::string labellen;

        labellen = readtostring(por);

        std::string label(std::strtol(labellen.c_str(), NULL, 30), '\0');
        label = readstring(label, por, label.size());

        if (debug)
          Rcout << label << std::endl;

        varlabels.push_back(label);

        varrec = readstring(varrec, por, varrec.size());
      }

    }

    Rcpp::List labtab;

    std::vector<std::string> labelsetnams;

    // D : value labels
    while (varrec.compare("D") == 0) {

      std::string unk1;
      unk1 = readtostring(por);

      std::string labelset;
      labelset = readtostring(por);

      std::string labelsetnam (std::strtol(labelset.c_str(), NULL, 30), '\0');
      labelsetnam = readstring(labelsetnam, por, labelsetnam.size());
      labelsetnams.push_back(labelsetnam);

      // Rcout << labelset << labelsetnam << std::endl;

      std::string labelnum;
      labelnum = readtostring(por);

      int labnums = std::strtol(labelnum.c_str(), NULL, 30);
      Rcpp::CharacterVector labvals(labnums);
      Rcpp::CharacterVector labtxts(labnums);


      for (int i = 0; i < labnums; ++i) {

        std::string labval;
        std::string labtxtlen;

        labval = readtostring(por);
        labtxtlen = readtostring(por);

        std::string labtxt (std::strtol(labtxtlen.c_str(), NULL, 30), '\0');
        labtxt = readstring(labtxt, por, labtxt.size());

        // Rcout << labtxt << std::endl;

        labvals[i] = labval;
        labtxts[i] = labtxt;

      }

      labvals.attr("names") = labtxts;

      labtab.push_back(labvals);
      labtab.attr("names") = labelsetnams;

      varrec = readstring(varrec, por, varrec.size());

    }


    if (debug)
      Rcout << varrec << std::endl;

    int n = 0;

    bool eof = false;

    size_t data_begin = por.tellg();

    // dry run until end of file is reached
    for (int kk = 0; kk < R_PosInf; ++kk) {
      for (int ii = 0; ii < vars; ++ii) {

        std::string val;
        val = readtostring(por);

        if (debug)
          Rcout << val << std::endl;

        eof = val.find_first_not_of("Z") == string::npos;

        if (eof) {
          Rcout << "End of file found. n is" << n << std::endl;
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

          // Rcpp::Rcout << val_s << std::endl;

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

    // 1. Create Rcpp::List
    Rcpp::List df(varnames.size());

    if ( varrec.compare("F") == 0) {

      for (int32_t i=0; i<varnames.size(); ++i)
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

      // fill list with data
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
            double val_g = 0.0;
            int    test = 0;

            // val_d = readfloat(val);

            char *cstr = new char[val.length() + 1];
            strcpy(cstr, val.c_str());

            // TDA function
            test = dnum(cstr, val_g);


            val_d = val_g;

            // FixMe: If I delete this R dies
            // delete [] cstr;

            // ToDo: PSPP states that a dot is not required for a missing
            if (val.compare("*.") == 0)
              val_d = NA_REAL;


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

            // Rcpp::Rcout << val_s << std::endl;
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

    return(df);


  } else {
    return (-1);
  }
}
