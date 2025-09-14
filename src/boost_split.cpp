/*
 * Copyright (C) 2018-2025 Jan Marvin Garbuszus
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

#include "spss.h"

//' split character vector at "="
//'
//' @param val_s CharacterVector
//' @import Rcpp
//' @keywords internal
//' @noRd
// [[Rcpp::export]]
Rcpp::CharacterVector boost_split(std::string val_s) {

  std::vector<std::string> vec_r;

  vec_r = split(val_s, "=", true);

  return(Rcpp::wrap(vec_r));
}
