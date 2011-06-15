; *******************************************************************
; Polydefix stress, strain, and texture analysis for experiment in 
; angle dispersive geometry
; Copyright (C) 2000-2011 S. Merkel, Universite Lille 1
; http://merkel.zoneo.net/Multifit/
; 
; This program is free software; you can redistribute it and/or
; modify it under the terms of the GNU General Public License
; as published by the Free Software Foundation; either version 2
; of the License, or (at your option) any later version.
; 
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
;
; You should have received a copy of the GNU General Public License
; along with this program; if not, write to the Free Software
; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
;
; *******************************************************************


PRO FitSubPatternObject__DEFINE 
	struct = { FitSubPatternObject, peakprofile: 0, npeaks: 0, ndelta: 0, widthfactor: 4.0, deltarange: PTR_NEW(), twotheta: PTR_NEW(), intensity: PTR_NEW(), hwidth: PTR_NEW(), weightGL: PTR_NEW(), limits: PTR_NEW(), bgcoefs: PTR_NEW()} 
END

; Basic init method
function FitSubPatternObject::Init
return, 1
end

; Cleanup method
pro FitSubPatternObject::Cleanup
end

function FitSubPatternObject::nPeaks
return, self.npeaks
end

function FitSubPatternObject::getDelta, i
return, (*self.deltarange)
end

function FitSubPatternObject::getIntensity, i
return, (*self.intensity)(i,*)
end

function FitSubPatternObject::getTheta, i
return, (*self.twotheta)(i,*)
end

function FitSubPatternObject::getHalfWidth, i
return, (*self.hwidth)(i,*)
end


; Lattice strain fit for one peak with no center correction
function FitSubPatternObject::latticeStrainFit, i, wavelength
cosdelta = cos(!PI*(*self.deltarange)/180.)
costheta = cos(!PI*(*self.twotheta)(i,*)/360.)
y = 1./sin(!PI*(*self.twotheta)(i,*)/360.)
x = 1.-3*costheta*costheta*cosdelta*cosdelta
sigma = 0.0005*(y)
guess = [mean(y),0.0]
fit = MPFITFUN('LATTICESTRAIN', x, y, sigma, guess, perror=perror, /quiet)
d0 = (fit[0]*wavelength)/2.
Q = fit[1]
dd0 = (perror[0]*wavelength)/2.
dQ = perror[1]
return, [d0, dd0, Q, dQ]
end


; Build a synthethic dataset from the fit we have
; Send
;    + value of azimtuh
;    + twotheta array
;    + number of two theta angles
; If we have no fit for this value of alpha, we just send back an array of 0's
; Otherwise, array of numbers
function FitSubPatternObject::syntheticdata, alpha,twotheta, ntheta
spectrum = fltarr(ntheta)
; Find the index for azimuth
tmp = WHERE((*(self.deltarange)) EQ alpha, count)
if (count lt 1) then return, spectrum ; No corresponding alpha
indexalpha = tmp[0]
; where are we in 2theta?
tmp = WHERE (twotheta GT (*self.limits)(indexalpha,0), count) 
if (count lt 1) then return, spectrum ; No corresponding 2theta
tmp = WHERE (twotheta LT (*self.limits)(indexalpha,1), count) 
if (count lt 1) then return, spectrum ; No corresponding 2theta
tmp = WHERE(twotheta GT (*self.limits)(indexalpha,0), count)
if (count eq 0) then indexL = 0 else indexL = tmp[0]
tmp = WHERE(twotheta  LT (*self.limits)(indexalpha,1), count)
if (count eq 0) then indexR = N_ELEMENTS(twotheta)-1 else indexR = tmp[count-1]
; Build a model
; background
spectrum(indexL:indexR) = spectrum(indexL:indexR) + (*self.bgcoefs)(indexalpha,0) + (*self.bgcoefs)(indexalpha,1)*twotheta(indexL:indexR)
; peaks
for peak=0, self.nPeaks-1 do begin
	inten = (*self.intensity)(peak,indexalpha)
	pos = (*self.twotheta)(peak,indexalpha)
	hwidth = (*self.hwidth)(peak,indexalpha)
	if (self.peakprofile eq 2) then begin
		weight = 0.0 ; Lorentz
	endif else if (self.peakprofile eq 1) then begin
		weight = (*self.weightGL)(peak,indexalpha) ; Voigt
	endif else begin
		weight = 1.0 ; Gauss
	end
	u = ((twotheta(indexL:indexR)-pos)/hwidth)^2
	lorentz =  inten / (u + 1)
	gauss = inten * exp (- 0.5 * u)
	spectrum(indexL:indexR) = spectrum(indexL:indexR) + weight*gauss + (1-weight)*lorentz
endfor
return, spectrum
end
 

; Read previous fit from Ascii file
function FitSubPatternObject::readFromascii, lun
	on_ioerror, bad
	; Number of peaks
	row = readascii(lun, com='#')
	self.nPeaks = fix(row)
	; Peak profile
	self.peakprofile = fix(readascii(lun,  com='#'))
	; Width factor 
	self.widthfactor = float(readascii(lun,  com='#'))
	; Azimuth angles
	self.ndelta = fix(readascii(lun,  com='#'))
	;print, 'Npeaks = ' + STRING(self.nPeaks, /PRINT)
	;print, 'Ndelta = ' + STRING(self.ndelta, /PRINT)
	self.deltarange=PTR_NEW(intarr(self.ndelta))
	for i=0, self.ndelta-1 do (*self.deltarange)(i) = fix(readascii(lun,  com='#'))
	; Setting up arrays
	self.twotheta=PTR_NEW(fltarr(self.npeaks,self.ndelta))
	self.intensity=PTR_NEW(fltarr(self.npeaks,self.ndelta))
	self.hwidth=PTR_NEW(fltarr(self.npeaks,self.ndelta))
	self.weightGL=PTR_NEW(fltarr(self.npeaks,self.ndelta))
	self.limits=PTR_NEW(fltarr(self.ndelta,2))
	self.bgcoefs=PTR_NEW(fltarr(self.ndelta,2))
	; Background sides
	for i=0, self.ndelta-1 do begin
		row = strsplit(readascii(lun,  com='#'), /extract)
		(*self.limits)(i,0) = float(row[0])
		(*self.limits)(i,1) = float(row[1])
	endfor
	; Background coefficients
	for i=0, self.ndelta-1 do begin
		row = strsplit(readascii(lun,  com='#'), /extract)
		(*self.bgcoefs)(i,0) = float(row[0])
		(*self.bgcoefs)(i,1) = float(row[1])
	endfor
	; Peak positions, intensity, half-width, weight Gauss/Lorentz
	for peak=0, self.nPeaks-1 do begin
		for i=0, self.ndelta-1 do begin
			row = strsplit(readascii(lun,  com='#'), /extract)
			(*self.twotheta)(peak,i) = float(row[0])
			(*self.intensity)(peak,i) = float(row[1])
			(*self.hwidth)(peak,i) = float(row[2])
			(*self.weightGL)(peak,i) = float(row[3])
		endfor
	endfor
	return, 1
bad: return, !ERR_STRING
end