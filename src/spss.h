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


template <typename T>
static std::string readstring(std::string mystring, T& sav,
                              int nchar)
{

  if (!sav.read(&mystring[0], nchar))
    Rcpp::warning("char: a binary read error occurred");

  return(mystring);
}

template <typename T>
static std::string readtostring(T& sav)
{

  std::string res(1, '\0');
  res = readstring(res, sav, res.size());

  // run until EOF is reached. file does not end on "/" but "Z"
  while (sav.peek() != EOF)
  {

    std::string next(1, '\0');
    next = readstring(next, sav, next.size());

    if ( (res.compare("*") == 0) & (next.compare(".") == 0)) {
      // missing (combine so we can check for "*.")
      res = res + next;

      break;

    } else if (next.compare("/") == 0) {
      // slash reached return w/o slash

      break;

    } else {
      // all good read another value
      res = res + next;
    }

  }

  return(res);
}


#include <boost/algorithm/string/classification.hpp>
#include <boost/algorithm/string/split.hpp>

static double readfloat (std::string &str) {

  double intp = 0.0, frcp = 0.0, res = 0.0;
  std::vector<std::string> vec_r;

  // if is digit do this else strtol
  if (str.find(".") != std::string::npos) {

    boost::split(vec_r, str,
                 boost::is_any_of("."), boost::token_compress_on);


    intp = std::strtol(vec_r[0].c_str(), NULL, 30);
    frcp = std::strtol(vec_r[1].c_str(), NULL, 30);


    Rprintf("integer: %f\n", intp);
    Rprintf("fractal: %f\n", frcp);

    // ToDo: This is a guess
    res = intp * 30.0 + frcp;

    res *= pow(30.0, (double) -1);

  } else {

    res = std::strtol(str.c_str(), NULL, 30);
  }

  return(res);

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
static void writebin(T t, std::fstream& sav, bool swapit)
{
  if (swapit==1){
    T t_s = swap_endian(t);
    sav.write((char*)&t_s, sizeof(t_s));
  } else {
    sav.write((char*)&t, sizeof(t));
  }
}



template <typename T>
static void writestr(std::string val_s, T len, std::fstream& sav)
{

  std::stringstream val_stream;
  val_stream << std::left << std::setw(len) << std::setfill(' ') << val_s;
  std::string val_strl = val_stream.str();

  sav.write(val_strl.c_str(),val_strl.length());

}

static void debug(std::istream& sav, int size) {
  char * memblock;

  memblock = new char [size];
  sav.read (memblock, size);

  Rcpp::Rcout << memblock << std::endl;

  delete[] memblock;
}

#endif
