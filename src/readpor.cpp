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
    stop ("woops");
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
    Rcout << spss << std::endl;

    // 2
    spss = readstring(spss, por, spss.size());
    Rcout << spss << std::endl;

    // 3
    spss = readstring(spss, por, spss.size());
    Rcout << spss << std::endl;

    // 4
    spss = readstring(spss, por, spss.size());
    Rcout << spss << std::endl;

    // 5
    spss = readstring(spss, por, spss.size());
    Rcout << spss << std::endl;

    // Rcpp::stop("Debug!");
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

    Rcout << "Pos: " << por.tellg() << std::endl;
    // end of header -----------------------------------------------------------


    // version and date record
    std::string vers (1, '\0');
    vers = readstring(vers, por, vers.size());

    std::string filedatelen = string(1, '\0');
    filedatelen = readstring(filedatelen, por, filedatelen.size());

    std::string slash = string(1, '\0');
    readstring(slash, por, slash.size());

    std::string filedate (std::stoi(filedatelen), '\0');
    filedate = readstring(filedate, por, filedate.size());


    std::string filetimelen = string(1, '\0');
    filetimelen = readstring(filetimelen, por, filetimelen.size());

    readstring(slash, por, slash.size());

    std::string filetime (std::stoi(filetimelen), '\0');
    filetime = readstring(filetime, por, filetime.size());



    Rcout << vers << " " << filedate << " " << filetime << std::endl;

    // identification record
    std::string prodrec (1, '\0');
    prodrec = readstring(prodrec, por, prodrec.size());

    std::string prodlen (2, '\0');
    prodlen = readstring(prodlen, por, prodlen.size());

    readstring(slash, por, slash.size());

    // std::string prod (std::stoi(prodlen), '\0');
    std::string prod (std::stoi(prodlen)+20, '\0');
    prod = readstring(prod, por, prod.size());

    Rcout << prod << std::endl;

    // readstring(slash, por, slash.size());

    std::string rtype (1, '\0');
    rtype = readstring(rtype, por, rtype.size());


    // if (rtype == 4) {
      std::string varcount (1, '\0');
      varcount = readstring(varcount, por, rtype.size());
    // }

    Rcout << varcount << std::endl;

    readstring(slash, por, slash.size());
    rtype = readstring(rtype, por, rtype.size());

    std::string floatcount (1, '\0');
    floatcount = readstring(floatcount, por, floatcount.size());


    readstring(slash, por, slash.size());


    return 0;


  } else {
    return (-1);
  }
}
