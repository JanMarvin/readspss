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

#include <boost/algorithm/string/classification.hpp> // Include boost::for is_any_of
#include <boost/algorithm/string/split.hpp> // Include for boost::split

using namespace Rcpp;
using namespace std;

#include "spss.h"

//' Reads the binary SPSS file
//'
//' @param filePath The full systempath to the dta file you want to import.
//' @param debug print debug information
//' @import Rcpp
//' @export
// [[Rcpp::export]]
List sav(const char * filePath, const bool debug, std::string encStr)
{

  std::ifstream sav(filePath, std::ios::in | std::ios::binary);
  if (sav) {


    int32_t n = 0;
    int32_t k = 0;
    int32_t charcode = 0;
    bool is_spss = 0;
    bool swapit = 0;

    std::string na = "NA";

    bool noencoding = false;

    if (encStr.compare(na)==0){
      encStr = "";
      noencoding = true;
    }


    std::string empty = "";

    std::string spss (8, '\0');
    spss = readstring(spss, sav, spss.size(), encStr);
    is_spss = std::regex_match(spss, std::regex("^\\$FL2@\\(#\\)$"));

    if (!is_spss)
      throw std::range_error("Can not read this file. Is it no SPSS sav file?");

    // Textfield 1
    //  SPSS DATA FILE
    //  OS
    //  (Software) Version?
    std::string datalabel (56, '\0');
    datalabel = readstring(datalabel, sav, datalabel.size(), encStr);

    // trim additional whitespaces
    datalabel = std::regex_replace(datalabel,
                                   std::regex("^ +| +$|( ) +"), "$1");

    if (debug)
      Rcout << "Datalabel:" << datalabel << std::endl;

    int arch=0; // file format? should be 2
    arch = readbin(arch, sav, swapit);

    if (arch != 2 || arch != 3)
      swapit = 1;

    k = readbin(k, sav, swapit);

    if (debug)
      Rprintf("K: %d \n", k);

    // nothing?
    // Number of base 30 digits
    // Case weight variable
    // Number of string variables
    // Number of unkown types

    int32_t cflag=0, cwvariables = 0;

    cflag = readbin(cflag, sav, swapit); // cflag compression
    cwvariables = readbin(cwvariables, sav, swapit); // case weight variables

    n = readbin(n, sav, swapit);

    if (debug)
      Rprintf("N: %d \n", n);

    if (n <= 0)
      stop("Get out! File does not know how many observations there are.");

    double bias = 0; // 100: compression bias
    bias = readbin(bias, sav, swapit);

    if (bias!=100)
      Rcpp::stop("bias != 100. Stop.");

    // creation date 9 dd_mmm_yy
    std::string datestamp (9, '\0');
    datestamp = readstring(datestamp, sav, datestamp.size(), encStr);

    // creation time 8 hh:mm:ss
    std::string timestamp (8, '\0');
    timestamp = readstring(timestamp, sav, timestamp.size(), encStr);

    std::string filelabel (67, '\0');
    filelabel = readstring(filelabel, sav, filelabel.size(), encStr);


    filelabel = std::regex_replace(filelabel,
                                   std::regex("^ +| +$|( ) +"), "$1");

    std::vector<string> varnames;
    std::vector<string> vallabels;
    std::vector<int32_t> vartype;

    int8_t lablen = 0;
    int32_t lablen32 = 0;
    int32_t nolab = 0;
    int32_t rtype = 0;
    int32_t typeINT = 0;
    int32_t has_var_label;
    int32_t n_missing_values;
    int32_t printINT;
    int32_t writeINT;
    std::string name (8, '\0');

    int32_t vtype=0, vlflag=0, nmiss=0, var4=0, var5 = 0;


    Rcpp::List missings = Rcpp::List();
    Rcpp::List varlist = Rcpp::List();


    rtype = readbin(rtype, sav, swapit);
    // Rprintf("Zahl in durchgang %d: %d \n", i,rtype);

    int32_t i = 0;
    while (rtype == 2) {

      // skip 20 bytes or read 5 unks
      vtype = readbin(vtype, sav, swapit);      // Variable type
      vlflag = readbin(vlflag, sav, swapit);    // Label flag
      nmiss = readbin(nmiss, sav, swapit);

      // bits of int32_t define digits, width and type
      var4 = readbin(var4, sav, swapit);

      // print
      int8_t var41, var42, var43, var44;

      var41 = (int8_t)var4;
      var42 = (var4 >> 8);
      var43 = (var4 >> 16);
      var44 = (var4 >> 24);

      // write
      // 4 and 5 are most likely identical
      var5 = readbin(var5, sav, swapit);

      int8_t var51, var52, var53, var54;
      var51 = (int8_t)var5;
      var52 = (var5 >> 8);
      var53 = (var5 >> 16);
      var54 = (var5 >> 24);


      Rcpp::NumericMatrix varmat(1,11);

      // Store vars in matrix export as attr varmat
      varmat(0,0)  = vtype;
      varmat(0,1)  = vlflag;
      varmat(0,2)  = nmiss; // 1, 2, 3 or -1, -2, -3 (range)
      varmat(0,3)  = var41; // digits print format?
      varmat(0,4)  = var42; // field width
      varmat(0,5)  = var43; // format type
      varmat(0,6)  = var44; // not used
      varmat(0,7)  = var51; // digits write format?
      varmat(0,8)  = var52; // field width
      varmat(0,9)  = var53; // format type
      varmat(0,10) = var54; // not used

      varlist.push_back(varmat);

      // Rcpp::Rcout << vtype <<"; "<< vlflag <<"; " << nmiss <<"; " << unk4 << "; "<< unk5 << endl;

      vartype.push_back(vtype);


      std::string nvarname (8, '\0');
      // read variable name 8 bytes long upercase letters
      nvarname = readstring(nvarname, sav, nvarname.size(), encStr);

      // trim additional whitespaces
      nvarname = std::regex_replace(nvarname,
                                    std::regex("^ +| +$|( ) +"), "$1");

      varnames.push_back(nvarname);
      // Rcout << nvarname << std::endl;

      if(vlflag==1)
      {
        rtype = readbin(rtype, sav, swapit);

        // Wer kommt auf so eine Scheiße?
        // Max laenge laut interwebz: 255.

        int32_t origlen = rtype;

        if (!(rtype%4==0))
        {
          for(int i=1; i<4; ++i)
          {
            if ((rtype+i)%4==0)
              rtype = rtype+i;
          }
        }

        // Rprintf("%d \n", rtype);
        std::string vallabel (rtype, '\0');
        vallabel = readstring(vallabel, sav, vallabel.size(), encStr);


        // trim additional whitespaces on the right
        vallabel = std::regex_replace(vallabel,
                                      std::regex("^ +| +$|( ) +"), "$1");
        vallabels.push_back(vallabel);

        rtype = origlen;

        // -----------------------------------------
        //  missings
        //

        int8_t const nmisstype = std::abs(nmiss);

        // SPSS knows 5 different missing types. -3, -2 are range types. 1, 2,
        // 3 are discrete types. Range types have min range and max range. -3
        // has an additional discrete value.


        if (nmisstype > 0) {

          // missing values
          // Vector needs to be of size n+1, because the first value will be
          // nmisstype followed by nmisstype values
          Rcpp::NumericVector missing(nmisstype+1);
          Rcpp::CharacterVector missingV(nmisstype+1);

          double miss0 = 0;
          bool noNum = false;

          if (vtype != 0)
            noNum = true;

          for (int i = 0; i < nmisstype; ++i) {


            if (noNum) {
              // read string and compare to an empty string. if the string contains
              // binary data it will be empty
              std::string mV (8, ' ');
              mV = readstring(mV, sav, mV.size(), "");


              mV = std::regex_replace(mV, std::regex("^ +| +$|( ) +"), "$1");

              missingV(0) = nmiss;
              missingV(i+1) = mV;

            } else {

              miss0 = readbin(miss0, sav, swapit);

              missing(0) = nmiss;
              missing(i+1) = miss0;
            }

          }

          if (noNum)
            missings.push_back(missingV);
          else
            missings.push_back(missing);

        }
      }

      // while loop above ends with 999
      // do not read another byte if 999 was already reached
      if (rtype!=999)
        rtype = readbin(rtype, sav, swapit);

    }

    if (debug)
      Rcout << "-- end of header" << std::endl;


    Rcpp::List Labell_list = Rcpp::List();
    // how to determine length?

    Rcpp::List EoHList = Rcpp::List();
    Rcpp::List doc;
    Rcpp::CharacterVector enc(1);

    std::vector<std::string> lvname;
    std::vector<std::string> lstr;


    int32_t subtyp = 0, size = 0, count = 0;
    int32_t major = 0, minor = 0, rev = 0, macode = 0;
    int32_t floatp = 0, compr = 0, endian = 0;
    double sysmiss = 0, highest = 0, lowest = 0;
    int32_t measure = 0, width = 0, alignment = 0;

    while(rtype!=999 & rtype != 2)
    {
      Rcpp::checkUserInterrupt();

      // 3 reading variable labels
      // first int: 3
      // second int: number of labels
      // first double: value coresponding to label
      // first short: label size
      // first char: labeltext
      //
      // if second int >1 restart at double

      bool noNum = 0;

      while (rtype == 3) {

        nolab = readbin(nolab, sav, swapit);
        // Rprintf("Nolab: %d \n", nolab);
        Rcpp::CharacterVector label(nolab);
        Rcpp::NumericVector code(nolab);
        Rcpp::CharacterVector codeV(nolab);

        // for some reason codes can be either doubles or strings. since we do
        // not know which we want, we read everything as a string. compare it
        // to figure out, what kind of type we have.
        for (int i=0; i < nolab; ++i)
        {
          double coden = 0;

          // read string and compare to an empty string. if the string contains
          // binary data it will be empty
          std::string cV (8, ' ');
          cV = readstring(cV, sav, cV.size(), "");

          // check for characters in the string lets hope SPSS does not allow
          // characters starting with a numeric or special character
          noNum = std::regex_search(cV, std::regex("^[A-Za-z0-9]")) &&
            !std::regex_search(cV, std::regex("@$"));

            // if its a double, do a memcpy, else trim whitespaces
            if( noNum ) {
              cV = std::regex_replace(cV, std::regex("^ +| +$|( ) +"), "$1");

              // return something so that we can later create a factor
              if(cV.compare(empty) != 0)
                codeV(i) = cV;

            } else {

              memcpy(&coden , cV.c_str(), sizeof(double));
              code(i) = coden;
            }


            lablen = readbin(lablen, sav, swapit);
            // Rprintf("Lablen: %d \n", lablen);

            if (!((lablen+1)%8==0))
            {
              for(int i=1; i<8; ++i)
              {
                if (((lablen+1)+i)%8==0)
                  lablen = lablen+i;
              }
            }

            std::string lab (lablen, '\0');
            lab = readstring(lab, sav, lab.size(), encStr);

            lab = std::regex_replace(lab, std::regex("^ +| +$|( ) +"), "$1");

            label(i) = lab;
        }

        // prepare release
        code.attr("names") = label;
        codeV.attr("names") = label;

        if (noNum)
          Labell_list.push_back(codeV);
        else
          Labell_list.push_back(code);

        rtype = readbin(rtype, sav, swapit);
      }

      // label 4:
      // labels are combinded.
      // first int: 4
      // second int: number of combined labels
      // (second int) ints: unk
      int32_t nolabels = 0, unk2 = 0;
      while (rtype==4)
      {
        Rcpp::checkUserInterrupt();

        nolabels = readbin(nolabels, sav, swapit); // number of labels

        Rcpp::NumericVector EoHUnks(nolabels);


        for (int i=0; i<nolabels; ++i) {
          unk2 = readbin(unk2, sav, swapit); // unk

          EoHUnks(i) = unk2;
          // Rprintf("End of Header bytes: %d \n", unk2);

        }
        EoHList.push_back(EoHUnks);
        rtype = readbin(rtype, sav, swapit);
      }

      while (rtype==6)
      {
        Rcpp::checkUserInterrupt();

        int32_t nlines = 0; // number of lines
        Rcpp::CharacterVector Document(nlines);
        std::string document (80, '\0');

        // Rcout << " --- Documentation --- " << std::endl;
        for (int i = 0; i < nlines; ++i)
        {
          document = readstring(document, sav, document.size(), encStr);
          Document(i) = document;

          // Rcout << document << std::endl;
        }

        doc.push_back( Document );

        rtype = readbin(rtype, sav, swapit);

      }


      // there can be two of this sections
      // integer info
      // floating point info
      // vardispl paramenter
      while (rtype==7) {
        Rcpp::checkUserInterrupt();

        // Rcout << "gelesen!" << endl;

        // subtype integer: 3 / floating: 4 / varsyst: 11
        subtyp = readbin(subtyp, sav, swapit);
        size = readbin(size, sav, swapit);
        count = readbin(count, sav, swapit);


        // Rprintf("rtype: %d \n", rtype);
        // Rprintf("subtyp: %d \n", subtyp);
        // Rprintf("size: %d \n", size);
        // Rprintf("count: %d \n", count);

        if (subtyp == 3) {
          // Rcout << "-- subtyp 3" << endl;

          major = readbin(major, sav, swapit);   // major version
          minor = readbin(minor, sav, swapit);   // minor version
          rev = readbin(rev, sav, swapit);       // revision
          macode = readbin(macode, sav, swapit); // machine code
          floatp = readbin(floatp, sav, swapit); // floating point pre
          compr = readbin(compr, sav, swapit);   // compression
          endian = readbin(endian, sav, swapit); // endianness
          charcode = readbin(charcode, sav, swapit); // charcode


          if ((encStr.compare(empty) == 0) & (!noencoding))
            encStr = codepage(charcode);

          // Rcout << subtyp << "/" << size << "/" << count << "/" << major  << "/" << minor  << "/" << rev  << "/" << macode  << "/" << floatp << "/" << compr  << "/" << endian  << "/" << charcode << std::endl;

        } else if (subtyp == 4) {
          // Rcout << "-- subtyp 4" << endl;
          sysmiss = readbin(sysmiss, sav, swapit);  // sysmiss always 3
          highest = readbin(highest, sav, swapit);  // highest
          lowest = readbin(lowest, sav, swapit);    // lowest

          // Rcout << subtyp << "/" << size << "/" << count << "/" << sysmiss  << "/" << highest  << "/" << lowest << std::endl;

        } else if (subtyp == 7) {

          std::string data (size*count, '\0');

          // ignore this
          data = readstring(data, sav, data.size(), empty);

          // data.erase(std::remove(data.begin(),
          //                        data.end(),
          //                        '\0'),
          //                        data.end());
          //
          // Rcout << data << std::endl;
        } else if (subtyp == 8) {

          // subtyp contains binary information in a single long string
          // my example shows some kind of program

          std::string data (size*count, '\0');

          // ignore this
          data = readstring(data, sav, data.size(), empty);

          // data.erase(std::remove(data.begin(),
          //                        data.end(),
          //                        '\0'),
          //                        data.end());

        } else if (subtyp == 11) {
          // Rcout << "-- subtyp 11" << endl;

          for (int i=0; i < count/3; ++i) {
            measure = readbin(measure, sav, swapit);        // measure 1/nom 2/Ord 3/Metr
            width = readbin(width, sav, swapit);            // width
            alignment = readbin(alignment, sav, swapit);    // alignment

            // Rcout << subtyp << "/" << size << "/" << count << "/" << measure  << "/" << width  << "/" << alignment  <<  std::endl;
          }

        } else if (subtyp == 13) {
          // very long varnames
          // Rcout << "-- subtyp 13" << endl;

          std::string longvarname (count, '\0');
          longvarname = readstring(longvarname, sav, count, encStr);

          // // split string in c++
          // std::stringstream lss(longvarname);
          // std::istream_iterator<std::string> begin(lss);
          // std::istream_iterator<std::string> end;
          // std::vector<std::string> lvname(begin, end);
          // std::copy(lvname.begin(), lvname.end(),
          //           std::ostream_iterator<std::string>(std::clog, "\t"));

          boost::split(lvname, longvarname,
                       boost::is_any_of("\t"), boost::token_compress_on);


        } else if (subtyp == 14) {
          // very long strings
          // Rcout << "--- subtyp 14 ---" << endl;
          std::string longstring (count, '\0');
          longstring = readstring(longstring, sav, count, encStr);

          boost::split(lstr, longstring,
                       boost::is_any_of("\t"), boost::token_compress_on);


        } else if (subtyp == 16) {

          int32_t unk = 0;

          for (int i = 0; i<count; ++i) {

            unk = readbin(unk, sav, swapit);

            if (debug) Rprintf("sub 16: unk1 %d\n", unk);

            unk = readbin(unk, sav, swapit);
            if (debug) Rprintf("sub 16: unk2 %d\n", unk);

          }

          // std::string longstring (count, '\0');
          // // readstring(longstring, sav, count);
          //
          //
          // stop("debug");

        } else if (subtyp == 18) {

          // another subtyp containing only program output? looks like some kind
          // of data base
          std::string data (size*count, '\0');

          // ignore this
          data = readstring(data, sav, data.size(), empty);

          // Rcout << data << std::endl;

        } else if (subtyp == 20) {
          std::string encoding (count, '\0');
          encoding = readstring(encoding, sav, count, encStr);

          enc(0) = encoding;


        } else if (subtyp == 24) {

          // seems like xml? dataview table format
          std::string data (size*count, '\0');

          // ignore this
          data = readstring(data, sav, data.size(), empty);

          // Rcout << data << std::endl;

        } else {
          std::string data (size*count, '\0');

          // ignore this
          readstring(data, sav, data.size(), empty);

          Rcout << data << std::endl;

          Rcout << "unknown subtype " << subtyp << " detected" << std::endl;
        }


        rtype = readbin(rtype, sav, swapit);

      }

      // if (debug)
      //   Rprintf("rtype: %d \n", rtype);

    }

    if (encStr.compare(empty) != 0) {

      if (debug)
        Rcout << "encoding" << std::endl;

      Rcpp::Environment base("package:base");
      Rcpp::Function iconv = base["iconv"];


      for (int i=0; i<varnames.size(); ++i) {
        // Rcpp::Rcout << mystring << std::endl;
        varnames[i] = Rcpp::as<std::string>(
          iconv(varnames[i], Rcpp::Named("from",encStr), Rcpp::Named("to",""))
        );
      }

    }


    // Data Part -------------------------------------------------------------//

    if(rtype != 999)
      Rcpp::stop("Expected data part. Somethings wrong in this file.");

    // c++ vector to Rcpp Vector
    IntegerVector Vartype = wrap(vartype);
    CharacterVector Varnames = wrap(varnames);


    CharacterVector vnam = Varnames[Vartype >= 0];
    IntegerVector vtyp = Vartype[Vartype >= 0];
    int32_t kv = vnam.size();


    // wrangling around to get the length of the strings
    NumericVector vtyp2 = wrap(vtyp);
    NumericVector res = ceil( vtyp2 / 8);

    if (debug) {
      Rcout << vnam << std::endl;
      Rcout << vtyp << std::endl;
      Rcout << res << std::endl;
      Rcout << vtyp << std::endl;
      Rcout << Vartype << std::endl;
    }

    if (debug)
      Rprintf("-- Start: Data Part \n");

    // if (cflag) {
    //   kv = k;
    //   vtyp = vartype;
    //   vnam = varnames;
    // }

    // 1. Create Rcpp::List
    Rcpp::List df(kv);
    for (uint16_t i=0; i<kv; ++i)
    {
      int const type = vtyp[i];
      switch(type)
      {
      case 0:
        SET_VECTOR_ELT(df, i, NumericVector(no_init(n)));
        break;

      default:
        SET_VECTOR_ELT(df, i, CharacterVector(no_init(n)));
      break;
      }
    }

    // Rcout << vtyp << std::endl;
    // Rcout << vnam << std::endl;
    // Rprintf("kv: %d \n", kv);

    // Rcpp::stop("stop");

    int32_t unk8=0;
    unk8 = readbin(unk8, sav, swapit); // 0

    uint8_t val_b = 0;
    double val_d = 0;
    uint32_t cells = 0;
    cells = n*k;
    uint32_t remaining_cells = 0;

    remaining_cells = cells;


    Rcpp::NumericVector data(cells);
    Rcpp::CharacterVector type(cells);


    // data is read in 8 byte chunks. k*n/8 (data remains)
    double chunk = 0;



    int32_t nn = 0;
    int32_t kk = 0;
    bool eof = 0;


    int32_t iter = 0;


    if (debug) {
      Rprintf("cflag: %d\n", cflag);
    }


    if (cflag) {

      // for (int ii = 0; ii < res.size(); ++ii) {

      // int rr = res[ii];
      std::string start = "";
      //
      // Rprintf("ii: %d \n", rr);
      //
      // int jj = 0;



      int chunkdone = 0;

      int res_i = 0;
      int kk_i = 0;
      int32_t res_kk = 0;

      while (!eof /* jj < rr && !eof && !chunkdone*/ ) {
        // Rprintf("%d - %d\n", rr, jj);

        Rcpp::checkUserInterrupt();

        // data is stored rowwise-ish.

        // chunk is 8 bit long. it gives the structure of the data. If it contains
        // only uint8_t it stores 8 vals. If data contains doubles it stores a
        // 253 and the next 8 byte will be the double.

        // Rcpp::Rcout << "read chunk" << endl;


        chunk = readbin(val_d, sav, swapit);

        IntegerVector chunkvec(8);

        // therefor with respect to the required data structure (numerics and
        // strings) the data has to be read.
        // e.g. if there are 2 vals, in the first 8 bit may be 4 rows.

        union {
          double d;
          uint8_t byte[8];
        } u;

        u.d = chunk;

        // combine strings
        int16_t lastval = 0;

        // Rcout << "kk: " << kk << std::endl;

        // Rprintf("res: %d\n", res_kk);
        // Rprintf("res_i: %d\n", res_i);

        for (int8_t i=0; i<8; ++i)
        {

          val_b = u.byte[i];

          // chunk verarbeiten
          // 0 = Leer
          // 1:251 = gut!
          // jede 253 = es folgt ein double
          // Steht eine 253 im code, dann folgt die an der Stelle erwartete Zahl
          // im naechsten double block.

          // Rprintf("val_b: %d \n", val_b);

          // Rcpp::stop("break!");
          //
          int32_t len = 0;
          int32_t const type = vartype[kk_i];
          len = type;

          // if (debug) {
          //   Rprintf("val_b: %d - type: %d - kk: %d\n", val_b, type, kk);
          // }
          //
          // Rprintf("kk: %d \n", kk);
          // Rprintf("nn: %d \n", nn);

          if (kk_i == vartype.size()-1)
            kk_i = 0;
          else
            kk_i++;

          // Rprintf("val_b: %d \n", val_b);
          // Rprintf("type: %d \n", type);

          switch (val_b)
          {

          case 0:
            {
              break;
              // ignored
            }

          default: // (val_b >= 1 & val_b <= 251) {
            {

              switch(type)
            {

            case 0:
            {
              // Rprintf("- %d \n", val_b-100);
              REAL(VECTOR_ELT(df,kk))[nn] = val_b-100;
              break;
            }

            default:
            {

              if (len==-1 || (len !=0 && len !=8) )
                len = 8;

              std::string val_s (len, '\0');

              val_s = readstring(val_s, sav, val_s.size(), encStr);

              start.append( val_s );

              if ((res_i >= res_kk-1) || (res_i+1 == res_kk)) {
                // Rcpp::Rcout << start << std::endl;

                // trim additional whitespaces
                start = std::regex_replace(start,
                                           std::regex("^ +| +$|( ) +"), "$1");
                as<CharacterVector>(df[kk])[nn] = start;

                // reset start
                start = "";
                kk++;
                res_i = 0;
              } else {
                res_i++;
              }

              break;
            }

            }

              break;
            }

          case 252:
            {
              // Rcpp::Rcout << "## Debug ... 252" << std::endl;
              eof = true;
              break;

              break;
            }

          case  253:
            {
              //           Rcpp::Rcout << "## Debug ... 253" << std::endl;
              //           Rprintf("nn %d & kk %d \n", nn, kk);
              switch(type)
            {

            case 0:
            {
              val_d = readbin(val_d, sav, swapit);
              REAL(VECTOR_ELT(df,kk))[nn] = val_d;
              // Rprintf("%f \n", val_d);
              break;
            }

            default:
            {

              // spss length 1:251 indicate a string. the value is the string
              // size. obvious spss uses the size to determine the size of the
              // string. there are two possible problems.
              // 1. len can be 1:7 in this case we know the max string size of the
              // variable is less than 8 bit long. still the field to read is 8 bit
              // long.
              // 2. the string is spread across different internal strings. in this
              // case we know the max size, still have to read each 8bit field.
              // maybe the max size can be used to have a second opinion wheather
              // or not a field contains a numeric or character. Following fields
              // have len -1.

              if (len==-1 || (len !=0 && len !=8) )
                len = 8;

              std::string val_s (len, '\0');

              val_s = readstring(val_s, sav, val_s.size(), encStr);
              start.append( val_s );

              res_kk = res[kk];

              if ((res_i >= res_kk-1)) {
                // Rprintf("kk: %d; nn: %d; res_i %d; res_kk %d\n",
                //         kk, nn, res_i, res_kk);
                // Rcpp::Rcout << start << std::endl;

                // trim additional whitespaces
                start = std::regex_replace(start,
                                           std::regex("^ +| +$|( ) +"), "$1");
                as<CharacterVector>(df[kk])[nn] = start;

                // reset start
                start = "";
                kk++;
                res_i = 0;
              } else {
                res_i++;
              }

              // start.append( val_s );
              // jj++;


              break;
            }

            }

              break;
            }

          case 254:
            {
              // 254 indicates that string chunks read before should be interpreted
              // as a single string. This is currently handled in R.

              // std::string val_s = "";
              //
              // as<CharacterVector>(df[kk])[nn] = val_s;

              if (res_i > 0) {
              // Rprintf("kk: %d; nn: %d; res_i %d\n", kk, nn, res_i);
              // Rcpp::Rcout << start << std::endl;

              // trim additional whitespaces
              start = std::regex_replace(start,
                                         std::regex("^ +| +$|( ) +"), "$1");

              as<CharacterVector>(df[kk])[nn] = start;

              // reset start
              start = "";
              kk++;
              res_i = 0;
            } else if (type >= 0) {
              kk++;
            }

            break;
            }

          case 255:
            {
              // 255 is a missing value in spss files.
              //
              switch(type)
            {

            case 0:
            {
              // Rcout << NA_REAL << std::endl;
              REAL(VECTOR_ELT(df,kk))[nn] = NA_REAL;
              // kk++;
              break;
            }
            default:
            {
              as<CharacterVector>(df[kk])[nn] = NA_STRING;
              kk++;
              break;
            }
              break;
            }


            }

            // if (i == 7) {
            //   chunkdone = 1;
            //
            //   Rcout << "stop: chunkdone" << std::endl;
            // }
          }

          // Rprintf(" type: %d\n", type);

          if (type == 0)
            kk++;

          // Update kk iterator. If kk is k, update nn to start in next row.
          if (kk == kv) {
            nn++;

            // Rprintf("nn: %d", nn);
            // some files are not ended with 252, ensure that no out of bounds
            // error occures.
            if (nn == n) {
              eof = true;

              if (debug)
                Rcout << "stop: eof" << std::endl;

              break;
            }

            // reset k
            kk = 0;
          }

        }

      }

    } else {

      kk = 0;

      std::string val_s = "";

      for (int ii = 0; ii < n*kv; ++ii) {

        int32_t const type = vtyp[kk];

        switch(type)
        {

        case 0:
        {
          // Rcout << val_d << std::endl;

          val_d = NA_REAL;
          val_d = readbin(val_d, sav, swapit);
          REAL(VECTOR_ELT(df,kk))[nn] = val_d;
          break;
        }

        default:
        {

          double len = type;

          len = ceil(len/8) * 8;

          std::string val_s ((int32_t)len, '\0');
          val_s = readstring(val_s, sav, val_s.size(), encStr);

          // shorten the string to the actual size reported by SPSS
          val_s.erase(type, std::string::npos);

          // trim additional whitespaces
          val_s = std::regex_replace(val_s,
                                     std::regex("^ +| +$|( ) +"), "$1");

          // Rcpp::Rcout << val_s << std::endl;
          as<CharacterVector>(df[kk])[nn] = val_s;

          break;
        }

        }

        kk++;

        if (kk == kv) {
          nn++;
          kk = 0;
        }

        // Rprintf("ii: %d \n", ii);
        // Rprintf("kk: %d \n", kk);

      }
    }


    // 3. Create a data.frame
    R_xlen_t nrows = Rf_length(df[0]);
    df.attr("row.names") = IntegerVector::create(NA_INTEGER, nrows);
    df.attr("names") = vnam;
    df.attr("class") = "data.frame";

    // debug(sav, 10);

    if (debug)
      Rprintf("-- End: Data Part \n");




    // close file
    sav.close();


    df.attr("datalabel") = datalabel;
    df.attr("datestamp") = datestamp;
    df.attr("timestamp") = timestamp;
    df.attr("filelabel") = filelabel;
    df.attr("vallabels") = vallabels;
    df.attr("varnames") = Varnames;
    df.attr("vartypes") = Vartype;
    df.attr("vtype") = vtyp;
    df.attr("varmat") = varlist;
    df.attr("missings") = missings;
    df.attr("label") = Labell_list;
    df.attr("haslabel") = EoHList;
    df.attr("data") = data;
    df.attr("longstring") = lstr;
    df.attr("longvarname") = lvname;
    df.attr("cflag") = cflag;
    df.attr("res") = res;
    df.attr("endian") = endian;
    df.attr("compression") = compr;
    df.attr("charcode") = charcode;
    df.attr("doc") = doc;
    df.attr("encoding") = enc;
    df.attr("encStr") = encStr;
    df.attr("noencoding") = noencoding;



    return(df);

  } else {
    return (-1);
  }
}
