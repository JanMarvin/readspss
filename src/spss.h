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


// Part of TDA. Program for Transition Data Analysis, written by Goetz Rohwer.
// Copyright (C) 1989,1991-97 Goetz Rohwer. GPL-2
static int getdigit(char *p, int *err)
{
  *err = 0;
  if (*p >= '0' && *p <= '9')
    return((int)(*p - '0'));
  else if (*p >= 'A' && *p <= 'T')
    return((int)(10 + *p - 'A'));
  else
    *err = 1;

  /* for debugging */
  // Rcpp::Rcout << *p << std::endl;

  return(0);
}


// Part of TDA. Program for Transition Data Analysis, written by Goetz Rohwer.
// Copyright (C) 1989,1991-97 Goetz Rohwer. GPL-2
static
int dnum(char *p, double &x, int *mv)
{
  int err;
  int neg = 0;
  int pnt = 0;
  int ex  = 0;
  int k = 0;
  int n = 0;
  double man = 0.0;
  double mex = 0.0;
  char *q;
  int RSPCnt = 0;

  q = p;
  while (*p && *p == ' ') {
    RSPCnt--;
    p++;
  }

  *mv = 0;
  if (*p == '*') {                /* check for internal missing value */
    *mv = 1;
    RSPCnt -= 2;
    p += 2;
    x = NA_REAL;
    return(1);                   // return(p);
  }

  if (*p == '-') {
    neg = 1;
    RSPCnt--;
    p++;
  }
  while (*p && *p != '/') {

    if (*p == '.') {
      pnt = 1;
      n = k;
    }
    else if (*p == '+')
      ex = 1;
    else if (*p == '-')
      ex = -1;
    else {
      if (!ex) {
        man *= 30.0;
        man += (double) getdigit(p, &err);

        if (err) {
          Rprintf("Unk0: %d\n", q); // digit_err(q)
          return(0);                // return(NULL);
        }
        k++;
        if (k > 13)
          Rprintf("Warning: found entry with %2d (base-30) digits.\n",k);
      }
      else {
        mex *= 30.0;
        mex += (double) getdigit(p, &err);

        if (err) {
          Rprintf("Unk1: %d\n", q); // digit_err(q)
          return(0);                // return(NULL);
        }
      }
    }
    RSPCnt--;
    p++;
  }
  if (neg)
    man = -man;

  if (pnt) {
    k -= n;
    while (k--)
      man /= 30.0;
  }
  if (ex == 1)
    man *= pow(30.0,mex);
  else if (ex == -1)
    man /= pow(30.0,mex);
  x = man;

  RSPCnt--;
  // return(++p);

  return(1);
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

#endif
