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

using namespace Rcpp;
using namespace std;


template <typename T>
T readbin( T t , std::istream& sav, bool swapit)
{
  if (!sav.read ((char*)&t, sizeof(t)))
    Rcpp::stop("readbin: a binary read error occurred");
  if (swapit==0)
    return(t);
  else
    return(t);
}

static void readstring(std::string &mystring, std::istream& sav, int nchar)
{
  if (!sav.read(&mystring[0], nchar))
    Rcpp::warning("char: a binary read error occurred");
  // Rcout << mystring << std::endl;
}

void debug(std::istream& sav, int size) {
  char * memblock;

  memblock = new char [size];
  sav.read (memblock, size);

  Rcpp::Rcout << memblock << std::endl;

  delete[] memblock;
}

//' Reads the binary SPSS file
//'
//' @param filePath The full systempath to the dta file you want to import.
//' @import Rcpp
//' @export
// [[Rcpp::export]]
List spss(const char * filePath, const bool debug)
{

  std::ifstream sav(filePath, std::ios::in | std::ios::binary);
  if (sav) {

    std::string spss (4, '\0');

    int32_t n = 0;
    int32_t k = 0;


    readstring(spss, sav, spss.size());
    // Rprintf("%s \n", spss);

    // @(#)
    sav.seekg(4, std::ios::cur);

    // Textfield 1
    //  SPSS DATA FILE
    //  OS
    //  (Software) Version?
    std::string datalabel (56, '\0');
    readstring(datalabel, sav, datalabel.size());

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

    Rcpp::CharacterVector varnames(k);
    Rcpp::CharacterVector vallabels(k);
    Rcpp::IntegerVector vartype(k);
    std::string nvarname (8, '\0');

    int8_t lablen = 0;
    int32_t nolab = 0;
    int32_t rtype = 0;

    int32_t vtype=0, vlflag=0, nmiss=0, unk4=0, unk5 = 0;

    Rcpp::NumericMatrix unkmat(k,5);

    Rcpp::List missings = Rcpp::List();

    // rtype = readbin(rtype, sav, 0);
    for(int32_t i=0; i < k; ++i) {

      rtype = readbin(rtype, sav, 0);
      // Rprintf("Zahl in durchgang %d: %d \n", i,rtype);

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

        // Store unks in matrix export as attr unkmat
        unkmat(i,0) = vtype;
        unkmat(i,1) = vlflag;
        unkmat(i,2) = nmiss; // 1, 2, 3 or -1, -2, -3 (range)
        unkmat(i,3) = unk4; // print format?
        unkmat(i,4) = unk5; // write format?

        // Rcpp::Rcout << vtype <<"; "<< vlflag <<"; " << nmiss <<"; " << unk4 << "; "<< unk5 << endl;

        vartype(i) = vtype;

        // read variable name 8 bytes long upercase letters
        readstring(nvarname, sav, nvarname.size());
        varnames(i) = nvarname;
        // Rprintf("Varname: %s \n", nvarname);


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
          vallabels(i) = vallabel;

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
    }

    // while loop above ends with 999
    // do not read another byte if 999 was already reached
    if (rtype!=999)
      rtype = readbin(rtype, sav, 0);


    Rcpp::List Labell_list = Rcpp::List();
    // how to determine length?

    Rcpp::List EoHList = Rcpp::List();


    while(rtype!=999)
    {

      // 3 reading variable labels
      // first int: 3
      // second int: number of labels
      // first double: value coresponding to label
      // first short: label size
      // first char: labeltext
      //
      // if second int >1 restart at double
      while (rtype == 3) {
        nolab = readbin(nolab, sav, 0);
        // Rprintf("Nolab: %d \n", nolab);
        Rcpp::CharacterVector label(nolab);
        Rcpp::NumericVector code(nolab);

        for (int i=0; i < nolab; ++i)
        {
          double coden = 0;
          coden = readbin(coden, sav, 0);
          // Rcout << unkd0 << endl;
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

          label(i) = lab;
          code(i) = coden;

          // Rcout << "Label: " << lab << std::endl;
        }


        // export List with named numerics
        Rcpp::List Labeltable(0);
        code.attr("names") = label;
        Labeltable.push_back(code);

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
        int32_t nlines = 0; // number of lines
        Rcpp::CharacterVector Document(nlines);
        std::string document (80, '\0');

        // Rcout << " --- Documentation --- " << std::endl;
        for (int i = 0; i < nlines; ++i)
        {
          readstring(document, sav, document.size());
          Document(i) = document;

          // Rcout << document << std::endl;
        }

        rtype = readbin(rtype, sav, 0);

      }


      // there can be two of this sections
      // integer info
      // floating point info
      // vardispl paramenter
      while (rtype==7) {

        // Rcout << "gelesen!" << endl;

        int32_t subtyp = 0, size = 0, count = 0;
        int32_t major = 0, minor = 0, rev = 0, macode = 0;
        int32_t floatp = 0, compr = 0, endian = 0, charcode = 0;
        double sysmiss = 0, highest = 0, lowest = 0;
        int32_t measure = 0, width = 0, alignment = 0;

        // subtype integer: 3 / floating: 4 / varsyst: 11
        subtyp = readbin(subtyp, sav, 0);
        size = readbin(size, sav, 0);     // size always 4
        count = readbin(count, sav, 0);   // count always 8

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
          std::string longvarname (count, '\0');
          readstring(longvarname, sav, count);

          // Rcout << longvarname << std::endl;

        } else if (subtyp == 14) {
          // very long strings
          // Rcout << "--- subtyp 14 ---" << endl;
          std::string longstring (count, '\0');
          readstring(longstring, sav, count);

        } else {
          std::string data (size*count, '\0');

          readstring(data, sav, data.size());
        }


        rtype = readbin(rtype, sav, 0);

      }

    }

    // Data Part -------------------------------------------------------------//

    if (debug)
      Rprintf("-- Start: Data Part \n");

    // 1. Create Rcpp::List
    Rcpp::List df(k);
    for (uint16_t i=0; i<k; ++i)
    {
      int const type = vartype[i];
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



    int nn = 0;
    int kk = 0;
    bool eof = 0;

    while(!sav.eof() && !eof)
    {
      // data is stored rowwise-ish.
      // for (int jj = 0; jj < k; ++jj) {
      // for (int ii = 0; ii < n; ++ii) {

      // chunk is 8 bit long. it gives the structure of the data. If it contains
      // only uint8_t it stores 8 vals. If data contains doubles it stores a
      // 253 and the next 8 byte will be the double.

      // Rcpp::Rcout << "read chunk" << endl;
      chunk = readbin(val_d, sav, 0);

      // therefor with respect to the required data structure (numerics and
      // strings) the data has to be read.
      // e.g. if there are 2 vals, in the first 8 bit may be 4 rows.

      union {
        double d;
        uint8_t byte[8];
      } u;

      u.d = chunk;

      uint8_t prev = 0;
      for(int i=0; i<8; ++i)
      {
        prev = chunk;
        val_b = u.byte[i];

        // Rprintf("val_b ist %d\n", val_b);

        // chunk verarbeiten
        // 0 = Leer
        // 1:251 = gut!
        // jede 253 = es folgt ein double
        // Steht eine 253 im code, dann folgt die an der Stelle erwartete Zahl
        // im naechsten double block.


        if(val_b != 252 && val_b != 253 && val_b != 254 && val_b!=255)
        {

          int const type = vartype[kk];
          switch(type)
          {

          case 0:
          {
            REAL(VECTOR_ELT(df,kk))[nn] = val_b-100;
            // Rprintf("%f \n", val_d);
            break;
          }


          default:
          {
            int32_t len = 0;
            len = vartype[kk];

            if(len==-1 || (len !=0 && len !=8) )
              len = 8;

            std::string val_s (len, '\0');

            readstring(val_s, sav, val_s.size());
            // Rcpp::Rcout << val_s << std::endl;
            as<CharacterVector>(df[kk])[nn] = val_s;
            break;
          }

          }

        } else if (val_b ==253) {
          //           Rcpp::Rcout << "## Debug ... 253" << std::endl;
          //           Rprintf("nn %d & kk %d \n", nn, kk);

          int const type = vartype[kk];
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
            int32_t len = 0;
            len = vartype[kk];

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

            if(len==-1 || (len !=0 && len !=8) )
              len = 8;

            std::string val_s (len, '\0');

            readstring(val_s, sav, val_s.size());
            // Rcpp::Rcout << val_s << std::endl;
            as<CharacterVector>(df[kk])[nn] = val_s;
            break;
          }

          }


        } else if (val_b ==254) {
          // 254 indicates that string chunks read before should be interpreted
          // as a single string. This is currently handled in R.

          std::string val_s = "";
          as<CharacterVector>(df[kk])[nn] = val_s;


        } else if (val_b == 255) {
          // 255 is a missing value in spss files.

          int const type = vartype[kk];
          switch(type)
          {

          case 0:
          {
            REAL(VECTOR_ELT(df,kk))[nn] = NA_REAL;
            break;
          }
          default:
          {
            as<CharacterVector>(df[kk])[nn] = NA_STRING;
            break;
          }
          }

        } else if (val_b ==252) {
          // Rcpp::Rcout << "## Debug ... 252" << std::endl;
          eof = true;
          break;
        }

        // Update kk iterator. If kk is k, update nn to start in next row.
        kk++;
        if (kk == k) {
          nn++;

          // some files are not ended with 252, ensure that no out of bounds
          // error occures.
          if (nn == n) {
            eof = true;
            break;
          }

          // reset k
          kk = 0;
        }

        // finish chunk, restart
        if (i == 8)
          break;

      }

    }




    // 3. Create a data.frame
    R_xlen_t nrows = Rf_length(df[0]);
    df.attr("row.names") = IntegerVector::create(NA_INTEGER, nrows);
    df.attr("names") = varnames;
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
    df.attr("vartypes") = vartype;
    df.attr("unkmat") = unkmat;
    df.attr("missings") = missings;
    df.attr("label") = Labell_list;
    df.attr("EoHUnks") = EoHList;
    df.attr("data") = data;
    df.attr("vartype") = vartype;


    return(df);

  } else {
    return (-1);
  }
}
