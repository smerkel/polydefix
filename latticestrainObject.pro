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

PRO latticeStrainObject__DEFINE 
	struct = { latticeSTrainObject, set: 0, name: '', nhkl:0, h:PTR_NEW(), k:PTR_NEW(), l:PTR_NEW(), d0: PTR_NEW(), dd0: PTR_NEW(), Q: PTR_NEW(), dQ: PTR_NEW(), centercorrection: 0, centerx:0., centery:0., dcenterx:0., dcentery: 0., offsetcorrection: 0, offset: 0.0, doffset: 0.0}
END

function latticeStrainObject::Init
return, 1
end

pro latticeStrainObject::cleanup
if (self.set eq 1) then begin
PTR_FREE, self.h, self.k, self.l, self.d0, self.dd0, self.Q, self.dQ
endif
end

pro latticeStrainObject::setName, name
self.name=name
end

pro latticeStrainObject::setFitResultsNoCorr, peaks, h, k, l, offset, fitresults
self.nhkl=N_ELEMENTS(peaks)
self.h = PTR_NEW(intarr(self.nhkl))
self.k = PTR_NEW(intarr(self.nhkl))
self.l = PTR_NEW(intarr(self.nhkl))
self.d0 =  PTR_NEW(fltarr(self.nhkl))
self.dd0 =  PTR_NEW(fltarr(self.nhkl))
self.Q =  PTR_NEW(fltarr(self.nhkl))
self.dQ =  PTR_NEW(fltarr(self.nhkl))
self.set = 1
self.offsetcorrection = 0
self.offset = offset
self.doffset = 0.
self.centercorrection = 0
self.centerx = 0.
self.centery = 0.
self.dcenterx = 0.
self.dcentery = 0.
for i=0, self.nhkl-1 do begin
	(*self.h)[i] = h[peaks[i]]
	(*self.k)[i] = k[peaks[i]]
	(*self.l)[i] = l[peaks[i]]
	(*self.d0)[i] = fitresults.fit[2*i]
	(*self.dd0)[i] = fitresults.perror[2*i]
	(*self.Q)[i] = fitresults.fit[2*i+1]
	(*self.dQ)[i] = fitresults.perror[2*i+1]
endfor
end


pro latticeStrainObject::setFitResultsWithOffset, peaks, h, k, l, fitresults
self.nhkl=N_ELEMENTS(peaks)
self.h = PTR_NEW(intarr(self.nhkl))
self.k = PTR_NEW(intarr(self.nhkl))
self.l = PTR_NEW(intarr(self.nhkl))
self.d0 =  PTR_NEW(fltarr(self.nhkl))
self.dd0 =  PTR_NEW(fltarr(self.nhkl))
self.Q =  PTR_NEW(fltarr(self.nhkl))
self.dQ =  PTR_NEW(fltarr(self.nhkl))
self.set = 1
self.offsetcorrection = 1
self.offset = fitresults.fit[0]*180./!PI
self.doffset = fitresults.perror[0]*180./!PI
self.centercorrection = 0
self.centerx = 0.
self.centery = 0.
self.dcenterx = 0.
self.dcentery = 0.
for i=0, self.nhkl-1 do begin
	(*self.h)[i] = h[peaks[i]]
	(*self.k)[i] = k[peaks[i]]
	(*self.l)[i] = l[peaks[i]]
	(*self.d0)[i] = fitresults.fit[2*i+1]
	(*self.dd0)[i] = fitresults.perror[2*i+1]
	(*self.Q)[i] = fitresults.fit[2*i+2]
	(*self.dQ)[i] = fitresults.perror[2*i+2]
endfor
end


pro latticeStrainObject::setFitResultsWithCenter, peaks, h, k, l, offset, fitresults
self.nhkl=N_ELEMENTS(peaks)
self.h = PTR_NEW(intarr(self.nhkl))
self.k = PTR_NEW(intarr(self.nhkl))
self.l = PTR_NEW(intarr(self.nhkl))
self.d0 =  PTR_NEW(fltarr(self.nhkl))
self.dd0 =  PTR_NEW(fltarr(self.nhkl))
self.Q =  PTR_NEW(fltarr(self.nhkl))
self.dQ =  PTR_NEW(fltarr(self.nhkl))
self.set = 1
self.offsetcorrection = 0
self.offset = offset
self.doffset = 0.
self.centercorrection = 1
self.centerx = fitresults.fit[0]
self.centery = fitresults.fit[1]
self.dcenterx = fitresults.perror[0]
self.dcentery = fitresults.perror[1]
for i=0, self.nhkl-1 do begin
	(*self.h)[i] = h[peaks[i]]
	(*self.k)[i] = k[peaks[i]]
	(*self.l)[i] = l[peaks[i]]
	(*self.d0)[i] = fitresults.fit[2*(i+1)]
	(*self.dd0)[i] = fitresults.perror[2*(i+1)]
	(*self.Q)[i] = fitresults.fit[2*(i+1)+1]
	(*self.dQ)[i] = fitresults.perror[2*(i+1)+1]
endfor
end

function latticeStrainObject::getSet
return, self.set
end

function latticeStrainObject::getName
return, self.name
end

function latticeStrainObject::getnhkl
return, self.nhkl
end

function latticeStrainObject::getnuse
return, self.nhkl
end

function latticeStrainObject::getd
return, (*self.d0)
end

function latticeStrainObject::getdd
return, (*self.dd0)
end

function latticeStrainObject::getQ
return, (*self.Q)
end

function latticeStrainObject::getdQ
return, (*self.dQ)
end

function latticeStrainObject::geth
return, (*self.h)
end

function latticeStrainObject::getk
return, (*self.k)
end

function latticeStrainObject::getl
return, (*self.l)
end

function latticeStrainObject::getCenterX
return, self.centerx
end

function latticeStrainObject::getCenterY
return, self.centery
end

function latticeStrainObject::getCenterErrX
return, self.dcenterx
end

function latticeStrainObject::getCenterErrY
return, self.dcentery
end

function latticeStrainObject::getOffset
return, self.offset
end

function latticeStrainObject::getErrOffset
return, self.doffset
end

; returns the lattice strains as a function of delta, the azimuth
; angle on the image plate
function latticeStrainObject::getDvsDelta, delta, wavelength, peakindex, donotcorrectcenter
; there is a trick, because the correction to obtain the angle psi
; from the azimuth angle depends on d, that we want to calculate
; For now, I'll simply loop...
offset = self.offset*!PI/180.
if ((self.centercorrection eq 0) or (donotcorrectcenter eq 1)) then begin
	cosdelta2 = cos(!PI*delta/180.-offset)*cos(!PI*delta/180.-offset)
	d = (*self.d0)[peakindex]*(1.+(*self.Q)[peakindex]*(1.-3.*cosdelta2))
	for i=0, 5 do begin
		costheta2 = 1-(wavelength/(2.*d))^2
		cospsi2 = cosdelta2*costheta2
		d = (*self.d0)[peakindex]*(1.+(*self.Q)[peakindex]*(1.-3.*cospsi2))
	endfor
endif else begin ; off-center case
	; in system properly centered
	cosdelta = cos(!PI*delta/180.-offset)
	sindelta = sin(!PI*delta/180.-offset)
	cosdelta0 = cos(!PI*delta/180.)
	sindelta0 = sin(!PI*delta/180.)
	theta = asin(wavelength/(2.*(*self.d0)[peakindex]*(1.+(1.-3.*cosdelta*cosdelta)*(*self.Q)[peakindex])))
	for k=0, 10 do theta =  asin(wavelength/(2.*(*self.d0)[peakindex]*(1.+(1.-3.*(cos(theta)*cosdelta)^2)*(*self.Q)[peakindex])))
	; in system that is off-center
	mestwotheta = atan(tan(2.*theta) - self.centerx*cosdelta0 - self.centery*sindelta0)
	;print, 2.*th, mestwotheta
	d = wavelength/(2.*sin(mestwotheta/2.))
endelse
return, d
end