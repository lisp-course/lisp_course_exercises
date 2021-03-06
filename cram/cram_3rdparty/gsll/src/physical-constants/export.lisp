;; Export the physical constant symbols
;; Liam Healy 2009-05-28 22:53:27EDT export.lisp
;; Time-stamp: <2011-08-20 20:14:05UTC export.lisp>
;;
;; Copyright 2009 Liam M. Healy
;; Distributed under the terms of the GNU General Public License
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(in-package :gsl)

(export 
 '(+mksa-speed-of-light+
   +mksa-gravitational-constant+
   +mksa-plancks-constant-h+
   +mksa-plancks-constant-hbar+
   +mksa-astronomical-unit+
   +mksa-light-year+
   +mksa-parsec+
   +mksa-grav-accel+
   +mksa-electron-volt+
   +mksa-mass-electron+
   +mksa-mass-muon+
   +mksa-mass-proton+
   +mksa-mass-neutron+
   +mksa-rydberg+
   +mksa-boltzmann+
   +mksa-bohr-magneton+
   +mksa-nuclear-magneton+
   +mksa-electron-magnetic-moment+
   +mksa-proton-magnetic-moment+
   +mksa-molar-gas+
   +mksa-standard-gas-volume+
   +mksa-minute+
   +mksa-hour+
   +mksa-day+
   +mksa-week+
   +mksa-inch+
   +mksa-foot+
   +mksa-yard+
   +mksa-mile+
   +mksa-nautical-mile+
   +mksa-fathom+
   +mksa-mil+
   +mksa-point+
   +mksa-texpoint+
   +mksa-micron+
   +mksa-angstrom+
   +mksa-hectare+
   +mksa-acre+
   +mksa-barn+
   +mksa-liter+
   +mksa-us-gallon+
   +mksa-quart+
   +mksa-pint+
   +mksa-cup+
   +mksa-fluid-ounce+
   +mksa-tablespoon+
   +mksa-teaspoon+
   +mksa-canadian-gallon+
   +mksa-uk-gallon+
   +mksa-miles-per-hour+
   +mksa-kilometers-per-hour+
   +mksa-knot+
   +mksa-pound-mass+
   +mksa-ounce-mass+
   +mksa-ton+
   +mksa-metric-ton+
   +mksa-uk-ton+
   +mksa-troy-ounce+
   +mksa-carat+
   +mksa-unified-atomic-mass+
   +mksa-gram-force+
   +mksa-pound-force+
   +mksa-kilopound-force+
   +mksa-poundal+
   +mksa-calorie+
   +mksa-btu+
   +mksa-therm+
   +mksa-horsepower+
   +mksa-bar+
   +mksa-std-atmosphere+
   +mksa-torr+
   +mksa-meter-of-mercury+
   +mksa-inch-of-mercury+
   +mksa-inch-of-water+
   +mksa-psi+
   +mksa-poise+
   +mksa-stokes+
   +mksa-faraday+
   +mksa-electron-charge+
   +mksa-gauss+
   +mksa-stilb+
   +mksa-lumen+
   +mksa-lux+
   +mksa-phot+
   +mksa-footcandle+
   +mksa-lambert+
   +mksa-footlambert+
   +mksa-curie+
   +mksa-roentgen+
   +mksa-rad+
   +mksa-solar-mass+
   +mksa-bohr-radius+
   +mksa-newton+
   +mksa-dyne+
   +mksa-joule+
   +mksa-erg+
   +mksa-stefan-boltzmann-constant+
   +mksa-thomson-cross-section+
   +mksa-vacuum-permittivity+
   +mksa-vacuum-permeability+
   +mksa-debye++mksa-speed-of-light+
   +cgsm-gravitational-constant+
   +cgsm-plancks-constant-h+
   +cgsm-plancks-constant-hbar+
   +cgsm-astronomical-unit+
   +cgsm-light-year+
   +cgsm-parsec+
   +cgsm-grav-accel+
   +cgsm-electron-volt+
   +cgsm-mass-electron+
   +cgsm-mass-muon+
   +cgsm-mass-proton+
   +cgsm-mass-neutron+
   +cgsm-rydberg+
   +cgsm-boltzmann+
   +cgsm-bohr-magneton+
   +cgsm-nuclear-magneton+
   +cgsm-electron-magnetic-moment+
   +cgsm-proton-magnetic-moment+
   +cgsm-molar-gas+
   +cgsm-standard-gas-volume+
   +cgsm-minute+
   +cgsm-hour+
   +cgsm-day+
   +cgsm-week+
   +cgsm-inch+
   +cgsm-foot+
   +cgsm-yard+
   +cgsm-mile+
   +cgsm-nautical-mile+
   +cgsm-fathom+
   +cgsm-mil+
   +cgsm-point+
   +cgsm-texpoint+
   +cgsm-micron+
   +cgsm-angstrom+
   +cgsm-hectare+
   +cgsm-acre+
   +cgsm-barn+
   +cgsm-liter+
   +cgsm-us-gallon+
   +cgsm-quart+
   +cgsm-pint+
   +cgsm-cup+
   +cgsm-fluid-ounce+
   +cgsm-tablespoon+
   +cgsm-teaspoon+
   +cgsm-canadian-gallon+
   +cgsm-uk-gallon+
   +cgsm-miles-per-hour+
   +cgsm-kilometers-per-hour+
   +cgsm-knot+
   +cgsm-pound-mass+
   +cgsm-ounce-mass+
   +cgsm-ton+
   +cgsm-metric-ton+
   +cgsm-uk-ton+
   +cgsm-troy-ounce+
   +cgsm-carat+
   +cgsm-unified-atomic-mass+
   +cgsm-gram-force+
   +cgsm-pound-force+
   +cgsm-kilopound-force+
   +cgsm-poundal+
   +cgsm-calorie+
   +cgsm-btu+
   +cgsm-therm+
   +cgsm-horsepower+
   +cgsm-bar+
   +cgsm-std-atmosphere+
   +cgsm-torr+
   +cgsm-meter-of-mercury+
   +cgsm-inch-of-mercury+
   +cgsm-inch-of-water+
   +cgsm-psi+
   +cgsm-poise+
   +cgsm-stokes+
   +cgsm-faraday+
   +cgsm-electron-charge+
   +cgsm-gauss+
   +cgsm-stilb+
   +cgsm-lumen+
   +cgsm-lux+
   +cgsm-phot+
   +cgsm-footcandle+
   +cgsm-lambert+
   +cgsm-footlambert+
   +cgsm-curie+
   +cgsm-roentgen+
   +cgsm-rad+
   +cgsm-solar-mass+
   +cgsm-bohr-radius+
   +cgsm-newton+
   +cgsm-dyne+
   +cgsm-joule+
   +cgsm-erg+
   +cgsm-stefan-boltzmann-constant+
   +cgsm-thomson-cross-section+
   +num-fine-structure+
   +num-avogadro+
   +num-yotta+
   +num-zetta+
   +num-exa+
   +num-peta+
   +num-tera+
   +num-giga+
   +num-mega+
   +num-kilo+
   +num-milli+
   +num-micro+
   +num-nano+
   +num-femto+
   +num-atto+
   +num-zepto+
   +num-yocto+))
