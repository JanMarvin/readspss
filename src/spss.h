#ifndef SPSS_H
#define SPSS_H


#include <Rcpp.h>
#include <fstream>
#include <string>
#include "swap_endian.h"

template <typename T>
T readbin( T t , std::istream& sav, bool swapit)
{
  if (!sav.read ((char*)&t, sizeof(t)))
    Rcpp::stop("readbin: a binary read error occurred");
  if (swapit==0)
    return(t);
  else
    return(swap_endian(t));
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
    case 65001:
      res = "UTF-8";
      break;
  }

  return(res);
}



static std::string readstring(std::string mystring, std::istream& sav,
                              int nchar)
{

  if (!sav.read(&mystring[0], nchar))
    Rcpp::warning("char: a binary read error occurred");

  return(mystring);
}


template <typename T>
static T Riconv(T &mystring, std::string &encStr) {

  std::string empty = "";

  if (encStr.compare(empty) != 0) {

    Rcpp::Environment base("package:base");
    Rcpp::Function iconv = base["iconv"];

    mystring = Rcpp::as<T>(
      iconv(mystring, Rcpp::Named("from", encStr), Rcpp::Named("to",""))
    );
  }

  return(mystring);

}

template <typename T>
static void writebin(T t, std::fstream& dta, bool swapit)
{
  if (swapit==1){
    T t_s = swap_endian(t);
    dta.write((char*)&t_s, sizeof(t_s));
  } else {
    dta.write((char*)&t, sizeof(t));
  }
}



template <typename T>
static void writestr(std::string val_s, T len, std::fstream& dta)
{

  std::stringstream val_stream;
  val_stream << std::left << std::setw(len) << std::setfill(' ') << val_s;
  std::string val_strl = val_stream.str();

  dta.write(val_strl.c_str(),val_strl.length());

}

static void debug(std::istream& sav, int size) {
  char * memblock;

  memblock = new char [size];
  sav.read (memblock, size);

  Rcpp::Rcout << memblock << std::endl;

  delete[] memblock;
}

#endif
