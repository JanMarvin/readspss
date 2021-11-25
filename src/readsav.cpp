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
#include <regex>

#include <boost/algorithm/string/classification.hpp>
#include <boost/algorithm/string/split.hpp>

using namespace Rcpp;

#include "spss.h"
#include "read_sav_known_n.h"
#include "read_sav_unknown_n.h"
#include "read_sav_uncompress.h"

//' Reads the binary SPSS file
//'
//' @param filePath The full systempath to the dta file you want to import.
//' @param debug print debug information
//' @param encStr encoding string
//' @param ownEnc encoding provided by localeToCharset
//' @import Rcpp
//' @export
// [[Rcpp::export]]
List readsav(const char * filePath, const bool debug, std::string encStr,
             std::string const ownEnc)
{

  std::ifstream sav(filePath, std::ios::in | std::ios::binary);
  if (sav) {

    bool is_spss = false, is_sav = false, is_zsav = false,
      ml_sav = false, ml_zsav = false,
      swapit = false, autoenc = false;

    int64_t n = 0;
    int32_t k = 0;

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
    spss = readstring(spss, sav);

    is_sav = std::regex_match(spss, std::regex("^\\$FL2@\\(#\\)$"));
    is_zsav = std::regex_match(spss, std::regex("^\\$FL3@\\(#\\)$"));
    ml_sav = std::regex_match(spss.substr(0,4), std::regex("^\\$FL2$"));
    ml_zsav = std::regex_match(spss.substr(0,4), std::regex("^\\$FL3$"));
    // most likely: "$FL2" can be followed by "SPSS"
    is_spss = (is_sav == true) || (is_zsav == true) ||
      (ml_sav == true) || (ml_zsav == true);



    if (!is_spss) {
      std::string fileheader(28, '\0');
      fileheader = readstring(fileheader, sav);

      fileheader = spss + fileheader;

      if (std::regex_search(fileheader, std::regex("ENCRYPTEDSAV")))
        stop("The file header indicates that this file is encrypted. "
              "A password is required to decode this file");

      throw std::range_error("Can not read this file. Is it no SPSS sav file?");
    }

    // Textfield 1
    //  SPSS DATA FILE
    //  OS
    //  (Software) Version?
    std::string datalabel (56, '\0');
    datalabel = readstring(datalabel, sav);

    // trim additional whitespaces
    datalabel = std::regex_replace(datalabel,
                                   std::regex("^ +| +$"), "$1");

    if (doenc) datalabel = Riconv(datalabel, encStr);

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

    n = readbin((int32_t)n, sav, swapit);

    if (debug)
      Rprintf("N: %d \n", n);

    int bias = 0; double biasd = 0; // 100: compression bias
    bias = readbin(biasd, sav, swapit);

    if ((cflag == 1) & (bias!=100))
      Rcpp::warning("Cflag = 1. Found bias = %d. Using this. Expected 100.",
                    bias);

    // creation date 9 dd_mmm_yy
    std::string datestamp (9, '\0');
    datestamp = readstring(datestamp, sav);

    // creation time 8 hh:mm:ss
    std::string timestamp (8, '\0');
    timestamp = readstring(timestamp, sav);

    std::string filelabel (67, '\0');
    filelabel = readstring(filelabel, sav);


    filelabel = std::regex_replace(filelabel,
                                   std::regex("^ +| +$"), "$1");

    if (doenc) filelabel = Riconv(filelabel, encStr);


    uint8_t lablen = 0;
    int32_t rtype = 0;

    std::string name (8, '\0');

    int32_t vtype=0, vlflag=0, nmiss=0, var4=0, var5 = 0, nolab = 0;
    int32_t subtyp = 0, size = 0, count = 0;
    int32_t major = 0, minor = 0, rev = 0, macode = 0;
    int32_t floatp = 0, compr = 0, endian = 0;
    int32_t measure = 0;
    int64_t unk = 0, bign = 0;

    double sysmiss = 0;
    double highest = R_PosInf, lowest = R_NegInf;


    Rcpp::List missings, varlist, Label_list, haslabel, doc,
    longmlist, longllist;
    Rcpp::CharacterVector longmissing;

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

      int32_t len = 0, nolabels = 0, lab_id = 0;

      if (debug)
        Rprintf("rtype %d ", rtype);

      if (rtype == 2) {

        // skip 20 bytes or read 5 unks
        vtype  = readbin(vtype, sav, swapit);     // Variable type
        vlflag = readbin(vlflag, sav, swapit);    // Label flag
        nmiss  = readbin(nmiss, sav, swapit);

        // bits of int32_t define digits, width and type
        // var4 = readbin(var4, sav, swapit);
        int8_t unk41, unk42, unk43, unk44;
        uint16_t unk416;

        unk41 = readbin(unk41, sav, swapit);
        unk42 = readbin(unk42, sav, swapit);
        unk43 = readbin(unk43, sav, swapit);
        unk44 = readbin(unk44, sav, swapit);
        // unk416 = readbin(unk416, sav, swapit);

        // Rprintf("%d %d %d %d\n", unk41, unk42, unk43, unk44);

        // 4 and 5 are most likely identical
        // var5 = readbin(var5, sav, swapit
        int8_t unk51, unk52, unk53, unk54;
        // int16_t unk516;

        unk51 = readbin(unk51, sav, swapit);
        unk52 = readbin(unk52, sav, swapit);
        unk53 = readbin(unk53, sav, swapit);
        unk54 = readbin(unk54, sav, swapit);
        // unk516 = readbin(unk516, sav, swapit);

        // Rprintf("%d %d %d %d\n", unk51, unk52, unk53, unk54);


        // Store vars in matrix export as attr varmat
        Rcpp::NumericMatrix varmat(1,11);

        varmat(0,0)  = vtype;
        varmat(0,1)  = vlflag;
        varmat(0,2)  = nmiss; // 1, 2, 3 or -1, -2, -3 (range)
        varmat(0,3)  = unk41; // digits print format?
        varmat(0,4)  = unk42; // field width
        varmat(0,5)  = unk43; // format type
        varmat(0,6)  = unk44; // not used
        // write
        varmat(0,7)  = unk51; // digits write format?
        varmat(0,8)  = unk52; // field width
        varmat(0,9)  = unk53; // format type
        varmat(0,10) = unk54; // not used

        if (vtype > -1) // -1 is of no further usage
          varlist.push_back(varmat);

        vartype.push_back(vtype);


        std::string nvarname (8, '\0');
        // read variable name 8 bytes long upercase letters
        nvarname = readstring(nvarname, sav);

        // trim additional whitespaces
        nvarname = std::regex_replace(nvarname,
                                      std::regex("^ +| +$"), "$1");

        varnames.push_back(nvarname);

        int32_t origlen = 0;
        if (vlflag==1)
        {
          origlen = readbin(origlen, sav, swapit);

          // Max length: 255.
          origlen = ceil((double)origlen/4) * 4;

          std::string vallabel (origlen, '\0');
          vallabel = readstring(vallabel, sav);


          // trim additional whitespaces on the right
          vallabel = std::regex_replace(vallabel,
                                        std::regex("^ +| +$"), "$1");

          if (vtype > -1) // -1 is of no further useage
            vallabels.push_back(vallabel);
        }

        // -----------------------------------------
        //  missings
        //

        int8_t const nmisstype = std::abs(nmiss);

        // SPSS knows 5 different missing types. -3, -2 are range types. 1, 2,
        // 3 are discrete types. Range types have min range and max range. -3
        // has an additional discrete value.


        if (debug) {
          Rcout << nvarname << " ";
          Rprintf("nmistype %d ", nmisstype);
          Rprintf("vflag %d\n", vlflag);
        }

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
              mV = readstring(mV, sav);


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

      }

      if (rtype == 3) {

        nolab = readbin(nolab, sav, swapit);

        // if (debug)
        //   Rprintf("nolab %d", nolab);

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
          cV = readstring(cV, sav);

          // check for characters in the string lets hope SPSS does not allow
          // characters starting with a numeric or special character
          noNum = std::regex_search(cV, std::regex("^[A-Za-z0-9]")) &&
            !std::regex_search(cV, std::regex("@$"));

            // if its a double, do a memcpy, else trim whitespaces
            if ( noNum ) {
              if (doenc) cV = Riconv(cV, encStr);
              cV = std::regex_replace(cV, std::regex("^ +| +$"), "$1");

              // return something so that we can later create a factor
              if (cV.compare(empty) != 0)
                codeV(i) = cV;

            } else {
              memcpy(&coden , cV.c_str(), sizeof(double));
              if (swapit) coden = swap_endian(coden);

              code(i) = coden;
            }


            lablen = readbin(lablen, sav, swapit);

            // +1 is whitespace caracter
            lablen = ( ceil((double)(lablen+1)/8) * 8 ) - 1;

            std::string lab (lablen, '\0');
            lab = readstring(lab, sav);
            lab = std::regex_replace(lab, std::regex("^ +| +$"), "$1");

            if (doenc) lab = Riconv(lab, encStr);

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

      }

      // label 4:
      // labels are combinded.
      // first int: 4
      // second int: number of combined labels
      // (second int) ints: unk
      if (rtype==4) {
        Rcpp::checkUserInterrupt();

        nolabels = readbin(nolabels, sav, swapit); // number of labels

        Rcpp::NumericVector haslab(nolabels);


        for (int i=0; i<nolabels; ++i) {
          lab_id = readbin(lab_id, sav, swapit); // unk

          haslab(i) = lab_id;

        }

        haslabel.push_back(haslab);
      }

      if (rtype==6) {
        Rcpp::checkUserInterrupt();

        int32_t nlines = 0; // number of lines

        nlines = readbin(nlines, sav, swapit);


        Rcpp::CharacterVector Document(nlines);
        std::string document (80, '\0');

        // Rcout << " --- Documentation --- " << std::endl;
        for (int i = 0; i < nlines; ++i)
        {
          std::string docline = readstring(document, sav);

          // if (doenc) docline = Riconv(docline, encStr);

          // trim additional whitespaces to the right
          docline = std::regex_replace(docline,
                                       std::regex(" +$"), "$1");

          Document(i) = docline;

        }

        doc.push_back( Document );

      }

      // additional information
      if (rtype==7) {
        Rcpp::checkUserInterrupt();

        // subtype integer: 3 / floating: 4 / varsyst: 11
        subtyp = readbin(subtyp, sav, swapit);
        size   = readbin(size, sav, swapit);
        count  = readbin(count, sav, swapit);

        if (debug)
          Rprintf("rtype %d; subtyp %d; size %d; count %d\n",
                  rtype, subtyp, size, count);

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

          if (debug) {
            Rprintf("\nsysmiss %fl\n", sysmiss);
            Rprintf("highest %d\n", highest);
            Rprintf("lowest %d\n\n", lowest);
          }

          // TODO: is this correct?
          if ( std::isnan(sysmiss) ) {
            lowest = highest;
            highest = R_PosInf;
          }

          break;
        }

        case 5:
        {
          // totals?
          totals = readstring(data, sav);

          break;
        }

          // case 6: // date info
        case 7: // PSPP : mrsets
        case 8: // my example shows some kind of program
          // case 12: // PSPP : UUID spotted twice only
        case 17: // PSPP : data file attribute
        case 18: // PSPP : variable attribute
        {
          // sav.seekg(size*count);

          // ignore this
          readstring(data, sav);

          break;
        }

        case 10:
        {

          extraproduct = readstringsize(data, sav, count);

          break;
        }


        case 11:
        {

          for (int32_t i = 0; i < count; ++i) {
          measure = readbin(measure, sav, swapit);

          disppar.push_back(measure);
        }

          break;
        }

        case 13:
        {
          longvarname = readstringsize(data, sav, count);

          break;
        }

        case 14:
        {
          longstring = readstringsize(data, sav, count);

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

          unk = readbin(unk, sav, swapit);
          bign = readbin(bign, sav, swapit);


          break;
        }

        case 20:
        {
          encoding = readstringsize(data, sav, count);

          if ((!noenc) & (charcode == 2) &
              (encoding.compare("windows-1252") == 0)) {
            encStr = "CP1252";
            autoenc = true;
            doenc = true;
          }

          break;
        }

        case 21:
        {
          auto end21pos = sav.tellg();
          end21pos += count;

          while(sav.tellg() < end21pos)
          {

          len = readbin(len, sav, swapit);
          std::string vn (len, '\0');

          vn = readstringsize(vn, sav, len);

          // Rprintf("vn %d \n", len);
          // Rcout << vn << std::endl;

          // 8 is the minimal value
          int32_t varw = 0, nvars = 0;
          varw = readbin(varw, sav, swapit);
          nvars = readbin(nvars, sav, swapit);

          // Rprintf("varw %d\n", varw);

          // set size
          CharacterVector longv(nvars);
          CharacterVector longl(nvars);

          for (int32_t i = 0; i<nvars; ++i){

            int32_t len1 = 0, len2 = 0;

            len1 = readbin(len1, sav, swapit);
            std::string val (len1, '\0');
            val = readstringsize(val, sav, len1);

            val = std::regex_replace(val, std::regex(" +$"), "$1");


            len2 = readbin(len2, sav, swapit);
            std::string lab (len2, '\0');
            lab = readstringsize(lab, sav, len2);

            // Rcout << val << " : "<< lab << std::endl;

            longv(i) = val;
            longl(i) = lab;
          }

          longv.attr("names") = longl;

          longlabvn.push_back(vn);
          longllist.push_back(longv);

          longllist.attr("names") = longlabvn;
          }

          break;
        }

        case 22:
        {
          // size = 1
          // count = 430

          auto endt22 = sav.tellg();
          endt22 += size * count;

          while (sav.tellg() < endt22) {

            len = readbin(len, sav, swapit);
            std::string vn (len, '\0');
            vn = readstringsize(vn, sav, len);

            int8_t mv = 0;
            mv = readbin(mv, sav, swapit);

            // set size
            CharacterVector longmissing(mv);
            len = readbin(len, sav, swapit); // should be 8

            if (debug)
              Rprintf("mv %d; len %d\n", mv, len);

            for (int32_t mm = 0; mm < mv; ++mm) {

              std::string val (len, '\0');
              val = readstring(val, sav);

              val = std::regex_replace(val, std::regex(" +$"), "$1");

              longmissing(mm) = val;
            }

            longmissvn.push_back(vn);
            longmlist.push_back(longmissing);
            longmlist.attr("names") = longmissvn;
          }


          break;
        }

        case 24:// seems like xml? dataview table format
        {
          // sav.seekg(size*count);

          // ignore this
          dataview = readstring(data, sav);

          break;
        }

        default:
        {
          // ignore this
          readstring(data, sav);

          Rcout << data << std::endl;

          Rcout << "unknown subtype " << subtyp << " detected." << std::endl;
          Rcout << "most likely no readson to worry. but if you want\n" <<
            "to help me out and can share a row of this datafile, \n" <<
              "please mail me!" << std::endl;

          break;
        }
        }

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

    if (rtype != 999)
      Rcpp::stop("Expected data part. Somethings wrong in this file.");

    int32_t unk8=0;
    unk8 = readbin(unk8, sav, swapit); // 0

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

    // prepare if zsav
    std::string sav_unc;

    if (is_zsav) {

      // uncompress to tempfile and close open uncompressed zsav file
      sav_unc = read_sav_uncompress(sav, swapit, cflag, debug);
      sav.close();

      // reopen zsav uncompressed data part as sav file
      // std::ifstream sav(sav_unc, std::ios::in | std::ios::binary);

      sav.open(sav_unc);
    }

    if (n < 0)
      n = read_sav_unknown_n(sav, swapit, cflag, debug,
                             kv, vtyp, res, vartype);

    // 1. Create Rcpp::List
    Rcpp::List df(kv);
    for (int32_t i=0; i<kv; ++i)
    {
      int const type = vtyp[i];
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

    if (n > 0)
      df = read_sav_known_n(df, sav, swapit, cflag, debug,
                            n, kv, vtyp, res, vartype, lowest, highest, bias);


    // close file
    sav.close();

    if (is_zsav) {
      // remove tempfile
      std::remove(sav_unc.c_str());
    }

    // encode full Character vector
    if (doenc & (n > 0)) {
      for (int32_t i=0; i<kv; ++i)
      {
        int const type = vtyp[i];

        // read and convert
        if (type > 0) {
          Rcpp::Environment base("package:base");
          Rcpp::Function iconv = base["iconv"];

          CharacterVector tmp = df[i];
          tmp = iconv(tmp, Rcpp::Named("from", encStr), Rcpp::Named("to",""));

          SET_VECTOR_ELT(df, i, tmp);
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
    df.attr("encoding") = encoding;
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
