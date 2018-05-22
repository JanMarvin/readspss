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

static void readstring(std::string &mystring, std::istream& sav, int nchar)
{
  if (!sav.read(&mystring[0], nchar))
    Rcpp::warning("char: a binary read error occurred");
  // Rcout << mystring << std::endl;
}

static void debug(std::istream& sav, int size) {
  char * memblock;

  memblock = new char [size];
  sav.read (memblock, size);

  Rcpp::Rcout << memblock << std::endl;

  delete[] memblock;
}

#endif
