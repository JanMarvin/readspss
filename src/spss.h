#ifndef SPSS_H
#define SPSS_H


#include <Rcpp.h>
#include <fstream>
#include <string>
#include "swap_endian.h"

struct info_t {
  Rcpp::IntegerVector vtyp;
  Rcpp::IntegerVector cc;
  Rcpp::IntegerVector itc;
  Rcpp::IntegerVector vartypes;
  Rcpp::IntegerVector vartyp;
  Rcpp::CharacterVector nvarnames;
  Rcpp::CharacterVector label;
  Rcpp::IntegerVector haslabel;
  Rcpp::List labtab;
  Rcpp::IntegerVector disppar;
};

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


inline const std::string codepage(int cp) {
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
inline std::string readstring(std::string &mystring, T& sav)
{

  if (!sav.read(&mystring[0], mystring.size()))
    Rcpp::stop("char: a binary read error occurred");

  return(mystring);
}


template <typename T>
inline std::string readstringsize(std::string &mystring, T& sav, int size)
{

  if (!sav.read(&mystring[0], size))
    Rcpp::stop("char: a binary read error occurred");

  return(mystring);
}

template <typename T>
inline std::string readtostring(T& sav)
{

  std::string res(1, '\0');
  res = readstring(res, sav);

  // run until EOF is reached. file does not end on "/" but "Z"
  while (sav.peek() != EOF)
  {

    std::string next(1, '\0');
    next = readstring(next, sav);

    if ( (res.compare("*") == 0) &&
         (((next.compare(".") == 0)) || (next.compare("1") == 0) ) ) {
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
inline int getdigit(char *p, int *err)
{
  *err = 0;
  int32_t z = 0;
  if (*p >= '0' && *p <= '9')
    z = (int32_t)(*p - '0');
  else if (*p >= 'A' && *p <= 'T')
    z = (int32_t)(10 + *p - 'A');
  else
    *err = 1;

  return(z);
}


// modified Part of TDA. Program for Transition Data Analysis, written by Goetz
// Rohwer.
// Copyright (C) 1989,1991-97 Goetz Rohwer. GPL-2
inline double dnum(std::string strng)
{
  double x = 0.0, man = 0.0, mex = 0.0; /* result, mantissa and exponent */
  int32_t err,  neg = 0, pnt = 0, ex  = 0, k = 0,  n = 0;
  char *p = &strng[0], *q;

  q = p;
  while (*p && *p == ' ') {
    p++;
  }

  if (*p == '*') {                /* check for internal missing value */
    p += 2;
    x = NA_REAL;
    return(x);
  }

  if (*p == '-') {
    neg = 1;
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
          Rcpp::stop("Unk0: %d\n", q);
          return(0);
        }
        k++;
        if (k > 13)
          Rcpp::stop("Warning: found entry with %2d (base-30) digits.\n",k);
      }
      else {
        mex *= 30.0;
        mex += (double) getdigit(p, &err);

        if (err) {
          Rcpp::stop("Unk1: %d\n", q);
          return(0);
        }
      }
    }
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
    man *= pow(30.0, mex);
  else if (ex == -1)
    man /= pow(30.0, mex);
  x = man;

  return(x);
}


/*--------------------------------------------------------------------------*/
/*  pnum1(fd,n)     print integer n to fd (base 30). Update SPSSPtr.        */

static char DIG30[] = {'0','1','2','3','4','5','6','7','8','9',
                       'A','B','C','D','E','F','G','H','I','J',
                       'K','L','M','N','O','P','Q','R','S','T'};

inline std::string pnum1(int32_t n)
{
  int32_t m, r;
  char *p, buf[100];
  std::string val_s;

  if (n < 0) {
    val_s +="-";
    n = -n;
  }
  p = buf;
  while (n >= 30) {
    m = (int32_t) (n / 30);
    r = n - 30 * m;
    snprintf(p++, 100, "%c",DIG30[r]);
    n = m;
  }
  val_s += DIG30[n];
  while (p > buf) {
    val_s += *--p;
  }

  return(val_s);
}

/*--------------------------------------------------------------------------*/
/*  pfnum(fd,x)     print floating point number x to fd (base 30).          */
/*                  Update SPSSPtr. Note: x >= 0.0                          */

inline std::string pfnum(double x)
{
  int32_t i;
  double a, b, c, d, e;
  double EPSI = std::numeric_limits<double>::epsilon();

  std::string val_s;

  if (x == 0)
    return ("0");

  e = floor(log(x) / log(30.0));
  b = x / pow(30.0, e);
  c = floor(b);
  if (c < 0.0 || c >= 30.0)
    Rcpp::stop("74"); // no clue what this supposed be

  val_s = DIG30[(int32_t)c];
  b -= c;
  if (b > EPSI) {
    val_s += ".";
    c = 30.0;
    for (i = 0; i < 10 ; ++i) {
      a = b * c;
      d = floor(a);
      val_s += DIG30[(int32_t)d];
      b -= d / c;
      if (b <= EPSI)
        break;
      c *= 30;
    }
  }
  i = (int32_t)e;
  if (i) {
    if (i < 0) {
      val_s += "-";
      i = -i;
    }
    else {
      val_s += "+";
    }
    val_s += pnum1(i);
  };

  return(val_s);
}

inline std::string linebreak(std::string& str)
{
  for (size_t i = 80; i < str.size(); i += 81)
    str.insert(i, "\n");

  return(str);
}

inline std::string writestr(std::string mystring, bool slash) {

  std::string val;

  val += pnum1(mystring.size());
  val += "/";
  val += mystring;

  if (slash)
    val += "/";

  return(val);

}


template <typename T>
inline T Riconv(T &mystring, std::string &encStr) {

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
inline T Riconv2(T &mystring, std::string &encStr) {

  std::string empty = "";

  if (encStr.compare(empty) != 0) {

    Rcpp::Environment base("package:base");
    Rcpp::Function iconv = base["iconv"];

    mystring = Rcpp::as<T>(
      iconv(mystring, Rcpp::Named("from", ""), Rcpp::Named("to",encStr))
    );
  }

  return(mystring);

}


template <typename T>
inline void writebin(T t, std::fstream& sav, bool swapit)
{
  if (swapit==1){
    T t_s = swap_endian(t);
    sav.write((char*)&t_s, sizeof(t_s));
  } else {
    sav.write((char*)&t, sizeof(t));
  }
}


inline void writestr(std::string val_s, int32_t len, std::fstream& sav)
{

  std::stringstream val_stream;
  val_stream << std::left << std::setw(len) << std::setfill(' ') << val_s;
  std::string val_strl = val_stream.str();

  sav.write(val_strl.c_str(),val_strl.length());

}

inline std::string b30str (std::string &val_s) {
  return std::to_string(std::strtol(val_s.c_str(), NULL, 30));
}

inline int32_t b30int (std::string &val_s) {
  return std::strtol(val_s.c_str(), NULL, 30);
}

#endif
