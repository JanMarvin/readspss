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
    std::string prodlen (1, '\0');
    prodlen = readstring(prodlen, por, prodlen.size());

    readstring(slash, por, slash.size());

    // strtoi as in R
    std::string prod (std::stol(prodlen, NULL, 30), '\0');
    prod = readstring(prod, por, prod.size());


    if (debug)
      Rcout << prod << std::endl;



    // optional
    // 3 : extra record
    varrec = readstring(varrec, por, varrec.size());

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
      std::string varsize (1, '\0');
      varsize = readstring(varsize, por, varsize.size());

      vars = std::strtol(varsize.c_str(), NULL, 30);

      if (debug)
        Rprintf("%d", vars);

      readstring(slash, por, slash.size());
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


    std::vector<int> vartypes;
    std::vector<std::string> varnames;
    std::string unkstr (1, '\0');


    // 7 : variable records
    while (varrec.compare("7") == 0)
    {

      // 0 or more (should read to next slash)
      std::string vartyp (1, '\0');
      vartyp = readstring(vartyp, por, vartyp.size());
      readstring(slash, por, slash.size());

      // vartype
      vartypes.push_back(std::atoi(vartyp.c_str()));


      // varnamelen (1 - 8)
      std::string varnamelen (1, '\0');
      varnamelen = readstring(varnamelen, por, varnamelen.size());
      readstring(slash, por, slash.size());

      std::string varname (std::atoi(varnamelen.c_str()), '\0');
      varname = readstring(varname, por, varname.size());

      // varname
      varnames.push_back(varname);

      // 5
      readstring(unkstr, por, unkstr.size());
      readstring(slash, por, slash.size());

      // 8
      readstring(unkstr, por, unkstr.size());
      readstring(slash, por, slash.size());

      // 2
      readstring(unkstr, por, unkstr.size());
      readstring(slash, por, slash.size());

      // 5
      readstring(unkstr, por, unkstr.size());
      readstring(slash, por, slash.size());

      // 8
      readstring(unkstr, por, unkstr.size());
      readstring(slash, por, slash.size());

      // 2
      readstring(unkstr, por, unkstr.size());
      readstring(slash, por, slash.size());

      // varrec
      varrec = readstring(varrec, por, varrec.size());


      if (debug) {
        Rcout << varname << std::endl;
        Rcout << varnamelen << std::endl;
      }

    }


    if (debug)
      Rcout << varrec << std::endl;

    int n = 2;

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
      for (int kk = 0; kk < n; ++kk) { // 2
        for (int ii = 0; ii < vars; ++ii) { // 2


          int const type = vartypes[ii];

          switch(type)
          {
          case 0:
          {
            std::string val (1, '\0');
            int val_i = 0;

            val = readstring(val, por, val.size());
            readstring(slash, por, slash.size());


            if (debug)
              Rcout << val << std::endl;

            val_i = std::strtol(val.c_str(), NULL, 30);

            REAL(VECTOR_ELT(df,ii))[kk] = val_i;

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

    return(df);


  } else {
    return (-1);
  }
}
