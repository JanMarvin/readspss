#ifndef SPSS_H
#define SPSS_H


#include <Rcpp.h>
#include <fstream>
#include <string>

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


static const std::string codepage(int cp) {
  std::string res;

  switch(cp) {
  case 874:
    res = "CP874";
    break;
  case 936:
    res = "CP936";
    break;
  case 1200:
    res = "UCS-2LE";
    break;
  case 1201:
    res = "UCS-2BE";
    break;
  case 1250:
    res = "CP1250";
    break;
  case 1251:
    res = "CP1251";
    break;
  case 1252:
    res = "CP1252";
    break;
  case 1253:
    res = "CP1253";
    break;
  case 1254:
    res = "CP1254";
    break;
  case 1255:
    res = "CP1255";
    break;
  case 1256:
    res = "CP1256";
    break;
  case 1257:
    res = "CP1257";
    break;
  case 1258:
    res = "CP1258";
    break;
  case 10000:
    res = "macroman";
    break;
  case 12000:
    res = "UCS-4LE";
    break;
  case 12001:
    res = "UCS-4BE";
    break;
  case 20127:
    res = "ASCII";
    break;
  case 20866:
    res = "koi8-r";
    break;
  case 21866:
    res = "koi8-u";
    break;
  case 28591:
    res = "latin1";
    break;
  case 28592:
    res = "latin2";
    break;
  case 28593:
    res = "latin3";
    break;
  case 28594:
    res = "latin4";
    break;
  case 28605:
    res = "latin-9";
    break;
  case 50221:
    res = "ISO-2022-JP";
    break;
  case 51932:
    res = "euc-jp";
    break;
  // case 65001:
  //   res = "UTF-8";
  //   break;
  }

  return(res);
}



static std::string readstring(std::string mystring, std::istream& sav,
                              int nchar, std::string encStr)
{

  if (!sav.read(&mystring[0], nchar))
    Rcpp::warning("char: a binary read error occurred");

  std::string empty = "";

  if (encStr.compare(empty) != 0) {

    // remove null termination from string
    mystring.erase(std::remove(mystring.begin(),
                               mystring.end(),
                               '\0'),
                               mystring.end());

    Rcpp::Environment base("package:base");
    Rcpp::Function iconv = base["iconv"];

    // Rcpp::Rcout << mystring << std::endl;
    mystring = Rcpp::as<std::string>(
      iconv(mystring, Rcpp::Named("from", encStr), Rcpp::Named("to",""))
    );
  }
  // Rcout << mystring << std::endl;

  return(mystring);
}

static void debug(std::istream& sav, int size) {
  char * memblock;

  memblock = new char [size];
  sav.read (memblock, size);

  Rcpp::Rcout << memblock << std::endl;

  delete[] memblock;
}

#endif
