/*
 * Copyright (C) 2018 Jan Marvin Garbuszus
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

#ifndef READ_SAV_KNOWN_N_H
#define READ_SAV_KNOWN_N_H


Rcpp::List read_sav_known_n (std::istream& sav,
               const bool swapit, const bool cflag,
               const bool debug, const bool doenc,
               const int32_t n, const int32_t kv,
               Rcpp::IntegerVector vtyp,
               Rcpp::NumericVector res,
               std::vector<int> vartype,
               const std::string encStr);

#endif
