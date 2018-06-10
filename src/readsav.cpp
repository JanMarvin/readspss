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
//' @import Rcpp
//' @export
// [[Rcpp::export]]
List sav(const char * filePath, const bool debug)
{

  std::ifstream sav(filePath, std::ios::in | std::ios::binary);
  if (sav) {

    std::string spss (4, '\0');

    int32_t n = 0;
    int32_t k = 0;


    readstring(spss, sav, spss.size());
    // Rprintf("%s \n", spss);

    // @(#) // todo add this as a check if not: bail out
    sav.seekg(4, std::ios::cur);

    // Textfield 1
    //  SPSS DATA FILE
    //  OS
    //  (Software) Version?
    std::string datalabel (56, '\0');
    readstring(datalabel, sav, datalabel.size());

    // trim additional whitespaces
    datalabel = std::regex_replace(datalabel,
                                   std::regex("^ +| +$|( ) +"), "$1");

    if (debug)
      Rprintf("Datalabel: %s \n", datalabel);

    int arch=0; // file format? should be 2
    arch = readbin(arch, sav, 0);

    k = readbin(k, sav, 0);

    if (debug)
      Rprintf("K: %d \n", k);

    // nothing?
    // Number of base 30 digits
    // Case weight variable
    // Number of string variables
    // Number of unkown types

    int32_t cflag=0, cwvariables = 0;

    cflag = readbin(cflag, sav, 0); // cflag compression
    cwvariables = readbin(cwvariables, sav, 0); // case weight variables

    n = readbin(n, sav, 0);

    if (n <= 0)
      stop("Get out! File does not know how many observations there are.");

    if (debug)
      Rprintf("N: %d \n", n);

    double bias = 0; // 100: compression bias
    bias = readbin(bias, sav, 0);

    if (bias!=100)
      Rcpp::stop("bias != 100. Stop.");

    // creation date 9 dd_mmm_yy
    std::string datestamp (9, '\0');
    readstring(datestamp, sav, datestamp.size());

    // creation time 8 hh:mm:ss
    std::string timestamp (8, '\0');
    readstring(timestamp, sav, timestamp.size());

    std::string filelabel (67, '\0');
    readstring(filelabel, sav, filelabel.size());

    std::vector<string> varnames;
    std::vector<string> vallabels;
    std::vector<int> vartype;

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

    int32_t vtype=0, vlflag=0, nmiss=0, unk4=0, unk5 = 0;


    Rcpp::List missings = Rcpp::List();
    Rcpp::List unklist = Rcpp::List();


    rtype = readbin(rtype, sav, 0);
    // Rprintf("Zahl in durchgang %d: %d \n", i,rtype);

    int32_t i = 0;
    while (rtype == 2) {

      if (rtype == 2)
      {
        // skip 20 bytes or read 5 unks
        vtype = readbin(vtype, sav, 0);      // Variable type
        vlflag = readbin(vlflag, sav, 0);    // Label flag
        nmiss = readbin(nmiss, sav, 0);
        // 0/1/-2/-3 Number of missing labels. positive its one number, negative a range?
        unk4 = readbin(unk4, sav, 0);
        // oder sind es int8_ts? aber 2 8 5 0 macht so viel Sinn wie 329730
        unk5 = readbin(unk5, sav, 0);        // 4 and 5 are equal.


        Rcpp::NumericMatrix unkmat(1,5);

        // Store unks in matrix export as attr unkmat
        unkmat(0,0) = vtype;
        unkmat(0,1) = vlflag;
        unkmat(0,2) = nmiss; // 1, 2, 3 or -1, -2, -3 (range)
        unkmat(0,3) = unk4; // print format?
        unkmat(0,4) = unk5; // write format?

        unklist.push_back(unkmat);

        // Rcpp::Rcout << vtype <<"; "<< vlflag <<"; " << nmiss <<"; " << unk4 << "; "<< unk5 << endl;

        vartype.push_back(vtype);


        std::string nvarname (8, '\0');
        // read variable name 8 bytes long upercase letters
        readstring(nvarname, sav, nvarname.size());

        // trim additional whitespaces
        nvarname = std::regex_replace(nvarname,
                                      std::regex("^ +| +$|( ) +"), "$1");

        varnames.push_back(nvarname);
        // Rcout << nvarname << std::endl;

        if(vlflag==1)
        {
          rtype = readbin(rtype, sav, 0);

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
          readstring(vallabel, sav, vallabel.size());


          // trim additional whitespaces on the right
          vallabel = std::regex_replace(vallabel,
                                        std::regex("^ +| +$|( ) +"), "$1");
          vallabels.push_back(vallabel);

          rtype = origlen;

          // Rprintf(" %d \n", rtype);
          // Rcout << "Vallabel:" << vallabel << std::endl;


          int8_t const nmisstype = std::abs(nmiss);

          // SPSS knows 5 different missing types. -3, -2 are range types. 1, 2,
          // 3 are discrete types. Range types have min range and max range. -3
          // has an additional discrete value.
          switch(nmisstype)
          {

          case 1:
          {
            // missing values
            Rcpp::NumericVector mOne(2);
            double miss0=0;

            miss0 = readbin(miss0, sav, 0); // missing value: 9

            mOne(0) = nmiss;
            mOne(1) = miss0;

            // Rcout << nmiss << ": "  << mOne(1) << endl;

            missings.push_back(mOne);
            break;

          }

          case 2:
          {
            // missing values
            Rcpp::NumericVector m2(3);
            double miss0=0, miss1=0;

            miss0 = readbin(miss0, sav, 0); // 1. missing value
            miss1 = readbin(miss1, sav, 0); // 2. missing value

            m2(0) = nmiss;
            m2(1) = miss0;
            m2(2) = miss1;

            // Rcout << nmiss << ": "  << m2(1) << " / " << m2(2)  << endl;

            missings.push_back(m2);
            break;
          }

          case 3:
          {
            // missing values
            Rcpp::NumericVector m3(4);
            double miss0=0, miss1=0, miss2=0;

            miss0 = readbin(miss0, sav, 0); // 1. missing value
            miss1 = readbin(miss1, sav, 0); // 2. missing value
            miss2 = readbin(miss2, sav, 0); // 3. missing value

            m3(0) = nmiss;
            m3(1) = miss0;
            m3(2) = miss1;
            m3(3) = miss2;

            // Rcout  << nmiss << ": " << m3(1) << " / " << m3(2) << " / " << m3(3) << endl;

            missings.push_back(m3);
            break;
          }

          }
        }
      }

      // while loop above ends with 999
      // do not read another byte if 999 was already reached
      if (rtype!=999)
        rtype = readbin(rtype, sav, 0);

    }

    if (debug)
      Rcout << "-- end of header" << std::endl;


    Rcpp::List Labell_list = Rcpp::List();
    // how to determine length?

    Rcpp::List EoHList = Rcpp::List();
    Rcpp::List lvarname;
    Rcpp::List lstring;

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

        nolab = readbin(nolab, sav, 0);
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
          std::string empty = "";
          std::string cV (8, ' ');
          readstring(cV, sav, cV.size());

          noNum = strcmp( cV.c_str(), empty.c_str());

          // if its a double, do a memcpy, else trim whitespaces
          if( !noNum ) {
            memcpy(&coden , cV.c_str(), sizeof(double));
            code(i) = coden;
          } else {
            cV = std::regex_replace(cV, std::regex("^ +| +$|( ) +"), "$1");
            codeV(i) = cV;
          }


          lablen = readbin(lablen, sav, 0);
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
          readstring(lab, sav, lab.size());

          lab = std::regex_replace(lab, std::regex("^ +| +$|( ) +"), "$1");

          label(i) = lab;
        }


        // export List with named numerics
        Rcpp::List Labeltable(0);
        code.attr("names") = label;
        codeV.attr("names") = label;

        if (!noNum)
          Labeltable.push_back(code);
        else
          Labeltable.push_back(codeV);


        Labell_list.push_back(Labeltable);

        rtype = readbin(rtype, sav, 0);
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

        nolabels = readbin(nolabels, sav, 0); // number of labels

        Rcpp::NumericVector EoHUnks(nolabels);


        for (int i=0; i<nolabels; ++i) {
          unk2 = readbin(unk2, sav, 0); // unk

          EoHUnks(i) = unk2;
          // Rprintf("End of Header bytes: %d \n", unk2);

        }
        EoHList.push_back(EoHUnks);
        rtype = readbin(rtype, sav, 0);
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
          readstring(document, sav, document.size());
          Document(i) = document;

          Rcout << document << std::endl;
        }

        rtype = readbin(rtype, sav, 0);

      }


      // there can be two of this sections
      // integer info
      // floating point info
      // vardispl paramenter
      while (rtype==7) {
        Rcpp::checkUserInterrupt();

        // Rcout << "gelesen!" << endl;

        int32_t subtyp = 0, size = 0, count = 0;
        int32_t major = 0, minor = 0, rev = 0, macode = 0;
        int32_t floatp = 0, compr = 0, endian = 0, charcode = 0;
        double sysmiss = 0, highest = 0, lowest = 0;
        int32_t measure = 0, width = 0, alignment = 0;

        // subtype integer: 3 / floating: 4 / varsyst: 11
        subtyp = readbin(subtyp, sav, 0);
        size = readbin(size, sav, 0);
        count = readbin(count, sav, 0);


        // Rprintf("rtype: %d \n", rtype);
        // Rprintf("subtyp: %d \n", subtyp);
        // Rprintf("size: %d \n", size);
        // Rprintf("count: %d \n", count);

        if (subtyp == 3) {
          // Rcout << "-- subtyp 3" << endl;

          major = readbin(major, sav, 0);   // major version
          minor = readbin(minor, sav, 0);   // minor version
          rev = readbin(rev, sav, 0);       // revision
          macode = readbin(macode, sav, 0); // machine code
          floatp = readbin(floatp, sav, 0); // floating point pre
          compr = readbin(compr, sav, 0);   // compression
          endian = readbin(endian, sav, 0); // endianness
          charcode = readbin(charcode, sav, 0); // charcode

          // Rcout << subtyp << "/" << size << "/" << count << "/" << major  << "/" << minor  << "/" << rev  << "/" << macode  << "/" << floatp << "/" << compr  << "/" << endian  << "/" << charcode << std::endl;

        } else if (subtyp == 4) {
          // Rcout << "-- subtyp 4" << endl;
          sysmiss = readbin(sysmiss, sav, 0);  // sysmiss always 3
          highest = readbin(highest, sav, 0);  // highest
          lowest = readbin(lowest, sav, 0);    // lowest

          // Rcout << subtyp << "/" << size << "/" << count << "/" << sysmiss  << "/" << highest  << "/" << lowest << std::endl;

        } else if (subtyp == 11) {
          // Rcout << "-- subtyp 11" << endl;

          for (int i=0; i < count/3; ++i) {
            measure = readbin(measure, sav, 0);        // measure 1/nom 2/Ord 3/Metr
            width = readbin(width, sav, 0);            // width
            alignment = readbin(alignment, sav, 0);    // alignment

            // Rcout << subtyp << "/" << size << "/" << count << "/" << measure  << "/" << width  << "/" << alignment  <<  std::endl;
          }

        } else if (subtyp == 13) {
          // very long varnames
          // Rcout << "-- subtyp 13" << endl;
          //
          std::string longvarname (count, '\0');
          readstring(longvarname, sav, count);

          lvarname.push_back( longvarname );

          // Rcout << longvarname << std::endl;

        } else if (subtyp == 14) {
          // very long strings
          // Rcout << "--- subtyp 14 ---" << endl;
          std::string longstring (count, '\0');
          readstring(longstring, sav, count);

          // remove null termination from string
          longstring.erase(std::remove(longstring.begin(),
                                       longstring.end(),
                                       '\0'),
                                       longstring.end());

          lstring.push_back( longstring );

          // std::cout << longstring << std::endl;

        } else {
          std::string data (size*count, '\0');

          readstring(data, sav, data.size());

          // Rcout << data << std::endl;
        }


        rtype = readbin(rtype, sav, 0);

      }

      if (debug)
        Rprintf("rtype: %d \n", rtype);

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
    unk8 = readbin(unk8, sav, 0); // 0

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


    Rcpp::List chunklist = Rcpp::List();

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

              readstring(val_s, sav, val_s.size());

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
              val_d = readbin(val_d, sav, 0);
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

              readstring(val_s, sav, val_s.size());
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
          val_d = readbin(val_d, sav, 0);
          REAL(VECTOR_ELT(df,kk))[nn] = val_d;
          break;
        }

        default:
        {

          double len = type;

          len = ceil(len/8) * 8;

          std::string val_s ((int32_t)len, '\0');
          readstring(val_s, sav, val_s.size());

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
    df.attr("vallabels") = vallabels;
    df.attr("varnames") = Varnames;
    df.attr("vartypes") = Vartype;
    df.attr("vtype") = vtyp;
    df.attr("unkmat") = unklist;
    df.attr("missings") = missings;
    df.attr("label") = Labell_list;
    df.attr("haslabel") = EoHList;
    df.attr("data") = data;
    df.attr("longstring") = lstring;
    df.attr("longvarname") = lvarname;
    df.attr("cflag") = cflag;
    df.attr("chunklist") = chunklist;
    df.attr("res") = res;


    return(df);

  } else {
    return (-1);
  }
}
