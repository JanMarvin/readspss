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

#include <boost/algorithm/string/classification.hpp>
#include <boost/algorithm/string/split.hpp>

using namespace Rcpp;
using namespace std;

#include "spss.h"

//' Reads the binary SPSS file
//'
//' @param filePath The full systempath to the dta file you want to import.
//' @param debug print debug information
//' @param encStr encoding string
//' @param ownEnc encoding provided by localeToCharset
//' @import Rcpp
//' @export
// [[Rcpp::export]]
List sav(const char * filePath, const bool debug, std::string encStr,
         std::string const ownEnc)
{

  std::ifstream sav(filePath, std::ios::in | std::ios::binary);
  if (sav) {

    bool is_spss = false, swapit = false, autoenc = false;

    int32_t n = 0, k = 0;
    int32_t charcode = 0, arch = 0;

    std::string na = "NA", empty = "";

    // by default encode. This changes if the user specified encoding FALSE
    bool doenc = true, noenc = 0;

    // if encStr == NA, set doenc = false and encStr to "" to avoid messing
    // with iconv
    if (encStr.compare(na)==0) {
      encStr = "";
      doenc = false, noenc = true;
    }
    if (encStr.compare(empty) == 0)
      doenc = false;


    std::string spss (8, '\0');
    spss = readstring(spss, sav, spss.size());
    is_spss = std::regex_match(spss, std::regex("^\\$FL2@\\(#\\)$"));

    if (!is_spss)
      throw std::range_error("Can not read this file. Is it no SPSS sav file?");

    // Textfield 1
    //  SPSS DATA FILE
    //  OS
    //  (Software) Version?
    std::string datalabel (56, '\0');
    datalabel = readstring(datalabel, sav, datalabel.size());

    // trim additional whitespaces
    datalabel = std::regex_replace(datalabel,
                                   std::regex("^ +| +$"), "$1");

    if(doenc) datalabel = Riconv(datalabel, encStr);

    if (debug)
      Rcout << "Datalabel:" << datalabel << std::endl;

    // file format? should be 2 or 3
    arch = readbin(arch, sav, swapit);

    if ((arch <2) | (arch > 3))
      swapit = true;

    if (debug)
      Rprintf("swapit: %d\n", swapit);

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

    if (bias!=100) Rcpp::stop("bias != 100. Stop.");

    // creation date 9 dd_mmm_yy
    std::string datestamp (9, '\0');
    datestamp = readstring(datestamp, sav, datestamp.size());

    // creation time 8 hh:mm:ss
    std::string timestamp (8, '\0');
    timestamp = readstring(timestamp, sav, timestamp.size());

    std::string filelabel (67, '\0');
    filelabel = readstring(filelabel, sav, filelabel.size());


    filelabel = std::regex_replace(filelabel,
                                   std::regex("^ +| +$"), "$1");

    if(doenc) filelabel = Riconv(filelabel, encStr);


    int8_t lablen = 0, div = 0;
    int32_t rtype = 0;

    std::string name (8, '\0');

    int32_t vtype=0, vlflag=0, nmiss=0, var4=0, var5 = 0, nolab = 0;
    int32_t subtyp = 0, size = 0, count = 0;
    int32_t major = 0, minor = 0, rev = 0, macode = 0;
    int32_t floatp = 0, compr = 0, endian = 0;
    int32_t measure = 0, width = 0, alignment = 0;
    double sysmiss = 0, highest = 0, lowest = 0;


    Rcpp::List missings, varlist, Label_list, haslabel, doc,
    longmlist, longllist;
    Rcpp::CharacterVector enc(1), longmissing;

    std::vector<std::string> lvname, lstr, varnames, vallabels,
    longmissvn, longlabvn;
    std::vector<int32_t> vartype, disppar;

    std::string longstring, longvarname, encoding, totals,
    dataview, extraproduct;

    rtype = readbin(rtype, sav, swapit);


    if (debug)
      Rprintf("rtype: %d\n", rtype);

    while( rtype < 999 )
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

      int32_t typeINT = 0, has_var_label = 0, n_missing_values = 0,
        printINT = 0, writeINT = 0, lablen32 = 0, len = 0;

      while (rtype == 2)
      {

        // skip 20 bytes or read 5 unks
        vtype  = readbin(vtype, sav, swapit);     // Variable type
        vlflag = readbin(vlflag, sav, swapit);    // Label flag
        nmiss  = readbin(nmiss, sav, swapit);

        // bits of int32_t define digits, width and type
        var4 = readbin(var4, sav, swapit);

        // print
        int8_t var41, var42, var43, var44;

        var41 = (int8_t)var4;
        var42 = (var4 >> 8);
        var43 = (var4 >> 16);
        var44 = (var4 >> 24);

        // 4 and 5 are most likely identical
        var5 = readbin(var5, sav, swapit);

        // write
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

        if (vtype > -1) // -1 is of no further useage
          varlist.push_back(varmat);

        vartype.push_back(vtype);


        std::string nvarname (8, '\0');
        // read variable name 8 bytes long upercase letters
        nvarname = readstring(nvarname, sav, nvarname.size());

        // trim additional whitespaces
        nvarname = std::regex_replace(nvarname,
                                      std::regex("^ +| +$"), "$1");

        varnames.push_back(nvarname);

        if (debug) {
          Rcout << nvarname << std::endl;
          Rprintf("vflag %d\n", vlflag);
          Rprintf("nmiss %d\n", nmiss);
          Rprintf("var41 %d\n", var41);
          Rprintf("var42 %d\n", var42);
          Rprintf("var43 %d\n", var43);
          Rprintf("var44 %d\n", var44);
        }

        int32_t origlen = 0;
        if(vlflag==1)
        {
          origlen = readbin(origlen, sav, swapit);

          // Max laenge laut interwebz: 255.

          if (!(origlen%4==0))
          {
            for(int i=1; i<4; ++i)
            {
              if ((origlen+i)%4==0)
                origlen = origlen + i;
            }
          }

          std::string vallabel (origlen, '\0');
          vallabel = readstring(vallabel, sav, vallabel.size());


          // trim additional whitespaces on the right
          vallabel = std::regex_replace(vallabel,
                                        std::regex("^ +| +$"), "$1");

          vallabels.push_back(vallabel);
        }

        // -----------------------------------------
        //  missings
        //

        int8_t const nmisstype = std::abs(nmiss);

        // SPSS knows 5 different missing types. -3, -2 are range types. 1, 2,
        // 3 are discrete types. Range types have min range and max range. -3
        // has an additional discrete value.


        if (debug)
          Rprintf("vflag %d\n", vlflag);

        // PSPP states that long strings are handled differently
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

          for (int32_t i = 0; i < nmisstype; ++i) {

            if (noNum) {
              std::string mV (8, '\0');
              mV = readstring(mV, sav, mV.size());


              mV = std::regex_replace(mV, std::regex("^ +| +$"), "$1");

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

        break;
      }

      while (rtype == 3) {

        nolab = readbin(nolab, sav, swapit);

        Rcpp::CharacterVector label(nolab), codeV(nolab);
        Rcpp::NumericVector code(nolab);

        // for some reason codes can be either doubles or strings. since we do
        // not know which we want, we read everything as a string. compare it
        // to figure out, what kind of type we have.
        for (int i=0; i < nolab; ++i)
        {
          double coden = 0;

          // read string and compare to an empty string. if the string contains
          // binary data it will be empty
          std::string cV (8, ' ');
          cV = readstring(cV, sav, cV.size());

          // check for characters in the string lets hope SPSS does not allow
          // characters starting with a numeric or special character
          noNum = std::regex_search(cV, std::regex("^[A-Za-z0-9]")) &&
            !std::regex_search(cV, std::regex("@$"));

            // if its a double, do a memcpy, else trim whitespaces
            if( noNum ) {
              if(doenc) cV = Riconv(cV, encStr);
              cV = std::regex_replace(cV, std::regex("^ +| +$"), "$1");

              // return something so that we can later create a factor
              if(cV.compare(empty) != 0)
                codeV(i) = cV;

            } else {
              memcpy(&coden , cV.c_str(), sizeof(double));
              if (swapit) coden = swap_endian(coden);

              code(i) = coden;
            }


            lablen = readbin(lablen, sav, swapit);

            if (!((lablen+1)%8==0))
            {
              for(int8_t i=1; i<8; ++i)
              {
                if (((lablen+1)+i)%8==0)
                  lablen = lablen+i;
              }
            }

            std::string lab (lablen, '\0');
            lab = readstring(lab, sav, lab.size());
            lab = std::regex_replace(lab, std::regex("^ +| +$"), "$1");

            if(doenc) lab = Riconv(lab, encStr);

            label(i) = lab;
        }

        // prepare release

        if (noNum){
          codeV.attr("names") = label;
          Label_list.push_back(codeV);
        } else{
          code.attr("names") = label;
          Label_list.push_back(code);
        }

        break;
      }

      // label 4:
      // labels are combinded.
      // first int: 4
      // second int: number of combined labels
      // (second int) ints: unk
      int32_t nolabels = 0, lab_id = 0;
      while (rtype==4)
      {
        Rcpp::checkUserInterrupt();

        nolabels = readbin(nolabels, sav, swapit); // number of labels

        Rcpp::NumericVector haslab(nolabels);


        for (int i=0; i<nolabels; ++i) {
          lab_id = readbin(lab_id, sav, swapit); // unk

          haslab(i) = lab_id;

        }
        haslabel.push_back(haslab);

        break;
      }

      while (rtype==6)
      {
        Rcpp::checkUserInterrupt();

        int32_t nlines = 0; // number of lines

        nlines = readbin(nlines, sav, swapit);


        Rcpp::CharacterVector Document(nlines);
        std::string document (80, '\0');

        // Rcout << " --- Documentation --- " << std::endl;
        for (int i = 0; i < nlines; ++i)
        {
          std::string docline = readstring(document, sav, document.size());

          if(doenc) docline = Riconv(docline, encStr);

          // trim additional whitespaces to the right
          docline = std::regex_replace(docline,
                                       std::regex(" +$"), "$1");

          Document(i) = docline;

        }

        doc.push_back( Document );


        break;
      }


      // additional information
      while (rtype==7)
      {
        Rcpp::checkUserInterrupt();

        // subtype integer: 3 / floating: 4 / varsyst: 11
        subtyp = readbin(subtyp, sav, swapit);
        size   = readbin(size, sav, swapit);
        count  = readbin(count, sav, swapit);

        std::string data (size*count, '\0');

        switch(subtyp)
        {

        case 3:
        {

          major = readbin(major, sav, swapit);   // major version
          minor = readbin(minor, sav, swapit);   // minor version
          rev = readbin(rev, sav, swapit);       // revision
          macode = readbin(macode, sav, swapit); // machine code
          floatp = readbin(floatp, sav, swapit); // floating point pre
          compr = readbin(compr, sav, swapit);   // compression
          endian = readbin(endian, sav, swapit); // endianness
          charcode = readbin(charcode, sav, swapit); // charcode

          // not forcefully userdefined or NA == ""
          if ((encStr.compare(empty) == 0) & (!noenc)) {
            encStr = codepage(charcode);

            // if a codepage was found, recode else not
            // do not encode if in matching encoding
            if ((encStr.compare(empty) != 0) & (encStr.compare(ownEnc) != 0)) {
              doenc = true;
              autoenc = true;
            }
          }

          break;
        }

        case 4:
        {
          sysmiss = readbin(sysmiss, sav, swapit);  // sysmiss always 3
          highest = readbin(highest, sav, swapit);  // highest
          lowest = readbin(lowest, sav, swapit);    // lowest

          break;
        }

        case 5:
        {
          // totals?
          totals = readstring(data, sav, data.size());

          break;
        }

        case 7: // mrsets
        case 8: // my example shows some kind of program
        case 17: // variable view
        case 18:
        {
          // sav.seekg(size*count);

          // ignore this
          readstring(data, sav, data.size());

          break;
        }

        case 10:
        {

          extraproduct = readstring(data, sav, count);

          break;
        }


        case 11:
        {

          // pspp tells this is the default
          div = 2;

          // calculate kkv later called kv number of cases
          int32_t kkv = count_if(vartype.begin(),vartype.end(), IsGreaterZero);

          if( count == kkv * 3 )
            div = 3;

          for (int32_t i = 0; i < count; ++i) {
            measure = readbin(measure, sav, swapit);

            disppar.push_back(measure);
          }

          break;
        }

        case 13:
        {
          longvarname = readstring(data, sav, count);

          break;
        }

        case 14:
        {
          longstring = readstring(data, sav, count);

          break;
        }

        case 16:
        {
          // unsure what this is about
          // count is 2
          // unk is a number
          // for count 2 unk appears to be N-obs: Did SPSS allow n > int32 at
          // some point of time and coud not adjust it in the header for back-
          // ward compatibilty?
          int64_t unk = 0;

          for (int i = 0; i<count; ++i) {

            unk = readbin(unk, sav, swapit);

            if (debug) Rprintf("sub 16: unk1 %d\n", unk);

          }

          break;
        }

        case 20:
        {
          encoding = readstring(data, sav, count);

          enc(0) = encoding;

          break;
        }

        case 21:
        {
          len = readbin(len, sav, swapit);
          std::string vn (len, '\0');

          vn = readstring(vn, sav, len);

          int32_t varw = 0, nvars = 0;
          varw = readbin(varw, sav, swapit);
          nvars = readbin(nvars, sav, swapit);

          // set size
          CharacterVector longv(nvars);
          CharacterVector longl(nvars);

          for (int8_t i = 0; i<nvars; ++i){

            int32_t len1 = 0, len2 = 0;

            len1 = readbin(len1, sav, swapit);
            std::string val (len1, '\0');
            val = readstring(val, sav, len1);

            val = std::regex_replace(val, std::regex(" +$"), "$1");


            len2 = readbin(len2, sav, swapit);
            std::string lab (len2, '\0');
            lab = readstring(lab, sav, len2);

            // Rcout << val << " : "<< lab << std::endl;

            longv(i) = val;
            longl(i) = lab;
          }

          longv.attr("names") = longl;

          longlabvn.push_back(vn);
          longllist.push_back(longv);

          longllist.attr("names") = longlabvn;

          break;
        }

        case 22:
        {
          len = readbin(len, sav, swapit);
          std::string vn (len, '\0');

          vn = readstring(vn, sav, len);

          int8_t mv = 0;
          mv = readbin(mv, sav, swapit);
          len = readbin(len, sav, swapit);

          // set size
          CharacterVector longmissing(mv);

          for (int8_t i = 0; i<mv; ++i){
            std::string mnV (len, '\0');

            mnV = readstring(mnV, sav, len);
            mnV = std::regex_replace(mnV, std::regex(" +$"), "$1");

            longmissing(i) = mnV;
          }

          longmissvn.push_back(vn);
          longmlist.push_back(longmissing);

          longmlist.attr("names") = longmissvn;


          break;
        }

        case 24:// seems like xml? dataview table format
        {
          // sav.seekg(size*count);

          // ignore this
          dataview = readstring(data, sav, data.size());

          break;
        }

        default:
        {
          // ignore this
          readstring(data, sav, data.size());

          Rcout << data << std::endl;

          Rcout << "unknown subtype " << subtyp << " detected." << std::endl;
          Rcout << "most likely no readson to worry. but if you want\n" <<
            "to help me out and can share a row of this datafile, \n" <<
              "please mail me!" << std::endl;

          break;
        }
        }

        break;
      }

      rtype = readbin(rtype, sav, swapit);

    }


    if (debug)
      Rcout << "-- end of header" << std::endl;

    // encStr should not be empty otherwise
    // the iconv call would be useless
    if (doenc & (encStr.compare(empty)!=0)) {

      if (debug)
        Rcout << "encoding" << std::endl;

      longstring  = Riconv(longstring, encStr);
      longvarname = Riconv(longvarname, encStr);
      varnames    = Riconv(varnames, encStr);
      vallabels   = Riconv(vallabels, encStr);

    }

    // split. could fail for some locales if encoding is suppressed
    boost::split(lstr, longstring,
                 boost::is_any_of("\t"), boost::token_compress_on);
    boost::split(lvname, longvarname,
                 boost::is_any_of("\t"), boost::token_compress_on);


    // Data Part -------------------------------------------------------------//

    if(rtype != 999)
      Rcpp::stop("Expected data part. Somethings wrong in this file.");

    // c++ vector to Rcpp Vector
    IntegerVector Vartype = wrap(vartype);
    CharacterVector Varnames = wrap(varnames);


    // select only numerics or the beginning of strings. This enables
    // reading into fewer columns and reduces the overhead in the R code
    CharacterVector vnam = Varnames[Vartype >= 0];
    IntegerVector vtyp = Vartype[Vartype >= 0];

    // if k is set to be the number of available numerics and string variables
    int32_t kv = vnam.size();


    // wrangling around to get the length of the strings
    NumericVector vtyp2 = wrap(vtyp);
    NumericVector res = ceil( vtyp2 / 8);

    if (debug) {
      Rcout << vnam << std::endl;
      Rcout << vtyp << std::endl;
      Rcout << res << std::endl;
    }

    if (debug)
      Rprintf("-- Start: Data Part \n");

    // if (debug)
    //   n = 1;


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

    int32_t unk8=0;
    unk8 = readbin(unk8, sav, swapit); // 0


    bool eof = 0;
    uint8_t val_b = 0;
    int32_t nn = 0, kk = 0;

    // data is read in 8 byte chunks. k*n/8 (data remains)
    double chunk = 0, val_d = 0;


    if (debug) {
      Rprintf("cflag: %d\n", cflag);
    }


    // cflag 1 = compression int8_t - bias
    if (cflag) {

      std::string start = "";
      int32_t res_i = 0, res_kk = 0, kk_i = 0;

      while (!eof) { // start data import until nn = n

        Rcpp::checkUserInterrupt();


        // data is stored rowwise-ish.

        // chunk is 8 bit long. it gives the structure of the data. If it contains
        // only uint8_t it stores 8 vals. If data contains doubles it stores a
        // 253 and the next 8 byte will be the double.

        chunk = readbin(val_d, sav, 0);

        IntegerVector chunkvec(8);

        // therefor with respect to the required data structure (numerics and
        // strings) the data has to be read.
        // e.g. if there are 2 vals, in the first 8 bit may be 4 rows.

        union {
          double d;
          uint8_t byte[8];
        } u;

        u.d = chunk;

        for (int8_t i=0; i<8; ++i)
        {

          val_b = u.byte[i];

          // 0 = empty
          // 1:251 = numeric/string
          // each 253 follow up on a string or double in next block

          int32_t len = 0;
          int32_t const type = vartype[kk_i];
          len = type;

          // kk_i is index of the original number of variables
          // kk_i is reset once kv the new number of varialbes is reachead
          ++kk_i;


          // if (debug) {
          //   Rprintf("val_b: %d - type: %d - kk: %d - nn: %d\n",
          //           val_b, type, kk+1, nn+1);
          //
          //   Rprintf("res_i: %d\n", res_i);
          // }


          // res_kk is the amount of chunks required to read until the
          // string is completely read
          res_kk = res[kk];

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
            REAL(VECTOR_ELT(df,kk))[nn] = val_b - 100;
            break;
          }

          default:
          {

            if (len==-1 || (len !=0 && len !=8) )
              len = 8;

            // beginning of a new string
            std::string val_s (len, '\0');
            val_s = readstring(val_s, sav, val_s.size());
            start.append( val_s );

            // if res_i == res_kk the full string was read and
            // can be written else continue the string
            if (res_i == res_kk-1) {

              // trim additional whitespaces to the right
              start = std::regex_replace(start,
                                         std::regex(" +$"), "$1");


              if(doenc) start = Riconv(start, encStr);
              as<CharacterVector>(df[kk])[nn] = start;

              // string completly written, reset start and res_i
              // and switch to next cell
              start = "";
              res_i = 0;
            } else {
              // string will be continued
              ++res_i;
            }

            break;
          }

          }

            break;
          }

          case 252:
          {
            // 252 should be end of file, but as many things
            // it is not required to be inside the file
            eof = true;
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
            val_s = readstring(val_s, sav, val_s.size());
            start.append( val_s );


            if (res_i == res_kk-1) {

              // trim additional whitespaces to the right
              start = std::regex_replace(start,
                                         std::regex(" +$"), "$1");

              if(doenc) start = Riconv(start, encStr);
              as<CharacterVector>(df[kk])[nn] = start;

              // reset
              start = "";
              res_i = 0;
            } else {
              ++res_i;
            }


            break;
          }

          }

            break;
          }

          case 254:
          {
            // 254 indicates that string chunks read before should be
            // interpreted as a single string.

            if (res_i == res_kk-1) {

            // trim additional whitespaces to the right
            start = std::regex_replace(start,
                                       std::regex(" +$"), "$1");

            if(doenc) start = Riconv(start, encStr);
            as<CharacterVector>(df[kk])[nn] = start;

            // reset start
            start = "";
            res_i = 0;
          } else {
            start.append("        ");
            ++res_i;
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
            break;
          }
          default:
          {
            as<CharacterVector>(df[kk])[nn] = NA_STRING;
            break;
          }
            break;
          }

          }
          }



          // variable is read
          if (res_i == 0)
            ++kk;

          // Update kk iterator. If kk is k, update nn to start in next row.
          if (kk == kv) {
            ++nn;

            // Rprintf("nn: %d", nn);
            // some files are not ended with 252, ensure that no out of bounds
            // error occures.
            if (nn == n) {
              eof = true;

              if (debug)
                Rcout << "stop: eof" << std::endl;

              break;
            }

            // always check if eof is reached
            eof = sav.eof();

            // reset k and res_kk
            kk = 0;
            kk_i = 0;
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
          val_s = readstring(val_s, sav, val_s.size());

          // shorten the string to the actual size reported by SPSS
          val_s.erase(type, std::string::npos);

          // trim additional whitespaces
          val_s = std::regex_replace(val_s,
                                     std::regex("^ +| +$"), "$1");

          if(doenc) val_s = Riconv(val_s, encStr);

          // Rcpp::Rcout << val_s << std::endl;
          as<CharacterVector>(df[kk])[nn] = val_s;

          break;
        }

        }

        ++kk;

        if (kk == kv) {
          ++nn;
          kk = 0;
        }

      }
    }


    // 3. Create a data.frame
    R_xlen_t nrows = Rf_length(df[0]);
    df.attr("row.names") = IntegerVector::create(NA_INTEGER, nrows);
    df.attr("names") = vnam;
    df.attr("class") = "data.frame";


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
    df.attr("disppar") = disppar;
    df.attr("missings") = missings;
    df.attr("label") = Label_list;
    df.attr("haslabel") = haslabel;
    df.attr("longstring") = lstr;
    df.attr("longvarname") = lvname;
    df.attr("longmissing") = longmlist;
    df.attr("longlabel") = longllist;
    df.attr("cflag") = cflag;
    df.attr("res") = res;
    df.attr("endian") = endian;
    df.attr("compression") = compr;
    df.attr("doc") = doc;
    df.attr("charcode") = charcode;
    df.attr("encoding") = enc;
    df.attr("encStr") = encStr;
    df.attr("ownEnc") = ownEnc;
    df.attr("doenc") = doenc;
    df.attr("autoenc") = autoenc;
    df.attr("swapit") = swapit;
    df.attr("totals") = totals;
    df.attr("dataview") = dataview;
    df.attr("extraproduct") = extraproduct;



    return(df);

  } else {
    return (-1);
  }
}
