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

; *************************************************** experimentObject ***********************
;
; Object to hold data for one experiment
; Variables
;   - tmp: integer used to transmit information from actions within the object itself
;   - directory: directory where the fit data are stored
;   - base: base name for fit data files
;   - first: number of first file
;   - last: number of last file
;   - digits: number of digits used in file names
;   - wavelength
;   - materialSet: 1 of material properties have been set, 0 otherwise
;   - material: object with material properties
;   - peaksSet: 1 of peak list has been set, 0 otherwise
;   - nhkl
;	- fitoffset: fit direction with maximum stress, if 1, do it, if 0 do not do it
;	- offset: direction with maximum stress (starting value if adjusted), in degrees
;	- fitcenter: fit position of beam center, if 1, do it, if 0 do not do it
;   - usepeak: array of integer, usepeak[i]=1 if peak n. i should be used in analysis
;   - h: array of integer, h Miller indices
;   - k: array of integer, k Miller indices
;   - l: array of integer, l Miller indices
;   - fitstrainsset: array of integer of length 'last', fitstrainsset[i]=1 if lattice strains
;          have been fitted for file i
;   - fitstrains: array of latticestrainObject of length 'last', fitstrains[i]=1 holds
;          lattice strains fit results for file i
;   - expdata: holds the actual experimetal data so we do not read them from the disk all the time and can perform operations on it...
;   - expdataset: array that tells if expdata[i] is set...


PRO experimentObject__DEFINE 
	struct = { experimentObject, tmp: 0, directory:'', base:'', first:0, last:0, digits:3, wavelength:0.0, materialSet:0, material: PTR_NEW(), peaksSet:0, nhkl:0, fitoffset:0, offset: 0., fitcenter:0 , outputP:0 ,usepeak: PTR_NEW(), h:PTR_NEW(), k:PTR_NEW(), l:PTR_NEW(), fitstrainsset: PTR_NEW(), fitstrains: PTR_NEW(), filelistset: 0, filelist: PTR_NEW(), fileindex: PTR_NEW(), expdata: PTR_NEW(), expdataset: PTR_NEW()}
END

; ************************************************************** Init ************************

; Init function
; Parameters:
;   - directory: directory where the fit data are stored
;   - base: base name for fit data files
;   - first: number of first file
;   - last: number of last file
;   - digits: number of digits used in file names
function experimentObject::Init, directory, base, first, last, digits
self.directory = directory
self.base = base
self.first = first
self.last = last
self.digits = digits
if (self.last gt 0) then begin
	self.fitstrains = PTR_NEW(OBJARR(self.last+1))
	self.fitstrainsset = PTR_NEW(intarr(self.last+1));
	self.expdata = PTR_NEW(OBJARR(self.last+1))
	self.expdataset = PTR_NEW(intarr(self.last+1));
	(*self.fitstrainsset)[*] = 0
	(*self.expdataset)[*] = 0
endif
self.material = PTR_NEW(OBJ_NEW('materialObject'))
return, 1
end

; *************************************************** Reset fits ****************************
; Functions to reset fits if important parameters have been changed (wavelength, fit options, peak list...)
; Can not be used if changing fit files because it is using the number of files
;
pro experimentObject::resetFit
if (self.last gt 0) then begin
	for i=0, self.last do $
		if ((*self.fitstrainsset)[i] eq 1) then OBJ_DESTROY, ((*self.fitstrains)[i])
	(*self.fitstrainsset)[*] = 0
endif
end


; *************************************************** Fitting options ***********************

; setFitOffset
;   - 1 to fit offsets, 0 otherwise
pro experimentObject::setFitOffset, fit
self.fitoffset = fit
self->resetFit
end

; getFitOffset
;   - 1 to fit offsets, 0 otherwise
function experimentObject::getFitOffset
return, self.fitoffset
end

; setOffset
;   - offset value in degrees
pro experimentObject::setOffset, offset
self.offset = offset
self->resetFit
end

; getOffset
;   - offset value in degrees
function experimentObject::getOffset
return, self.offset
end

; setFitCenter
;   - 1 to fit offsets, 0 otherwise
pro experimentObject::setFitCenter, fit
self.fitcenter = fit
self->resetFit
end

; getFitCenter
;   - 1 to fit offsets, 0 otherwise
function experimentObject::getFitCenter
return, self.fitcenter
end

; ********************************************* Lattice strains output options *******************
; added 16/01/2015 NH. 
; switches on/off pressure output along with lattice strains
; outputP = if 0 do not output pressure
pro experimentObject::setOutputP, outpP
self.outputP = outpP
self-> resetFit
end

; gets the option status 
;   if 0 do not output pressure
function experimentObject::getOutputP
return, self.outputP
end

; *************************************************** Wavelength functions ***********************

; setWavelength
;   - wave: wavelength, in Angstroms
pro experimentObject::setWavelength, wave
self.wavelength = wave
self->resetFit
end

; getWavelength
;   - returns: wavelength, in Angstroms
function experimentObject::getWavelength
return, self.wavelength
end

; ***************************************** Temporary variable functions ***********************

; Temporary variable
; usefull to transmit information from actions within the object itself...
pro experimentObject::setTmp, tmp
self.tmp = tmp
end

function experimentObject::getTmp
return, self.tmp
end

; *************************************************** Material functions ***********************

function experimentObject::getMaterial
return, (*self.material)
end

pro experimentObject::setMaterial, newmat
(*self.material) = newmat
self.materialSet = 1
self->resetFit
end

; *************************************************** Setting FIT Files  ***********************

; setFITFiles
; Parameters:
;   - dir: directory where the fit data are stored
;   - base: base name for fit data files
;   - first: number of first file
;   - last: number of last file
;   - digits: number of digits used in file names
function experimentObject::setFITFiles, dir, base, first, last, digits
if (self.last gt 0) then begin
	for i=0, self.last do $
		if ((*self.fitstrainsset)[i] eq 1) then OBJ_DESTROY, ((*self.fitstrains)[i])
	PTR_FREE, self.fitstrains
	PTR_FREE, self.fitstrainsset
endif
self.directory = dir
self.base = base
self.first = first
self.last = last
self.digits = digits
if (self.last gt 0) then begin
	self.fitstrains = PTR_NEW(OBJARR(self.last+1))
	self.expdata = PTR_NEW(OBJARR(self.last+1))
	self.fitstrainsset = PTR_NEW(intarr(self.last+1));
	self.expdataset = PTR_NEW(intarr(self.last+1));
	(*self.fitstrainsset)[*] = 0
	(*self.expdataset)[*] = 0
endif
test = self->setFileList()
return, 1
end

function experimentObject::setFileList
; it is not set, we will scan the fit files to find it...
nfiles = 0
for j=self.first, self.last do begin
	fileindex = intformat(j,self.digits)
	filename = strtrim(self.directory) + strtrim(self.base) + "_" + fileindex + ".fit"
	; print, 'Testing ' + filename
	if (FILE_TEST(filename) eq 1) then nfiles += 1
endfor
if (nfiles gt 0) then begin
	self.filelist = PTR_NEW(strarr(nfiles))
	self.fileindex = PTR_NEW(intarr(nfiles))
	index = 0
	for j=self.first, self.last do begin
		fileindex = intformat(j,self.digits)
		filename = strtrim(self.directory) + strtrim(self.base) + "_" + fileindex + ".fit"
		fileshort = strtrim(self.base) + "_" + fileindex
		; print, 'Testing ' + filename
		if (FILE_TEST(filename) eq 1) then begin
			(*self.filelist)[index] = fileshort
			(*self.fileindex)[index] = j
			index += 1
		endif
	endfor
	self.filelistset = 1
	return, 1
endif
self.filelistset = 1
return, "I did not find any of the fit files..."
end

; returns number of pictures
function experimentObject::getnumberPatterns
return, nPattern = N_ELEMENTS(*self.filelist)
end

; Returns an array with available files
function experimentObject::getFileIndex
; if it is set, we return it
if (self.filelistset ne 1) then test = self->setFileList()
return, (*self.fileindex)
end

; Returns a list of available datasets
function experimentObject::getFileList
; if it is set, we return it
if (self.filelistset ne 1) then test = self->setFileList()
return, (*self.filelist)
end

; Returns the index of a file, send the real index (between first and last), and it'll give you its index in the file list and index arrary (non existing files are removed, and it starts from 0)
function experimentObject::getRealFileIndex, i
if (self.filelistset eq 0) then return, -1
return, where((*self.fileindex) eq i)
end


; returns name for file index i
function experimentObject::getFileName, i
; if it is set, we return it
if (self.filelistset eq 0) then return, ''
return, (*self.filelist)[i]
end

function experimentObject::getNFitFiles
if (self.filelistset eq 0) then return, 0
return, N_ELEMENTS(*self.fileindex)
end

;**************************************************** Setting azimuth angles files **********************************


; Returns a list of available azimuth
function experimentObject::getAngleList
; if it is set, we return it
if (self.filelistset ne 1) then return, ''
; We get list of angles for first peak of first file
return, (self->getExperimentalData(0))->getDelta(0)
end

; returns name for angle index i
function experimentObject::getAngleName, i
; if it is set, we return it
if (self.filelistset eq 0) then return, ''
return, (*self.anglelist)[i]
end

; *************************************************** Setting and fetching HKL information *****

; There is a issue with peak numbering
; the internal arrays such as
;   - self.h
;   - self.k
;   - self.l
;   - self.used
; are numbered simply, with no trick.
;
; However, internal, many function need a peak number that does not count peaks that are not used...
; For instance, if you have
; npeaks = 3
; used[0] = 1, used[1] = 0, used[2] = 1
; h[0] = 1, h[1] = 1, h[2] = 1
; k[0] = 0, k[1] = 1, k[2] = 2
; l[0] = 0, l[1] = 1, l[2] = 2
; some function will want the miller indices of the second used peak, which corresponds to i=2
; and not i=1 (hkl for second used peak here is 122 while it is 111 for the second peak)
; therefore, all functions here have an option /USED
;
; With the above example
; - getK, 1 will return 1
; - getK, 1, /used will return 2


; Get number of HKL peaks
function experimentObject::getNHKL
; if it is set, we return it
if (self.peaksSet eq 1) then return, self.nhkl
; it is not set, we will scan the fit files to find it...
found = 0
theend = 0
j = self.first
while (theend eq 0) do begin
	if (j gt self.last) then theend = 1 else begin
		fileindex = intformat(j,self.digits)
		filename = strtrim(self.directory) + strtrim(self.base) + "_" + fileindex + ".fit"
		; print, 'Testing ' + filename
		if (FILE_TEST(filename) eq 1) then begin
			openr, lun, filename, /get_lun
			fitdata = OBJ_NEW('FitPatternObject')
			a = fitdata->readFromAscii(lun)
			free_lun, lun
			if (a ne 1) then begin
				tmp = DIALOG_MESSAGE("Error: " + a, /ERROR)
				return, 0
			endif
			self.nhkl = fitdata->getNPeaks()
			self.h = PTR_NEW(intarr(self.nhkl))
			self.k = PTR_NEW(intarr(self.nhkl))
			self.l = PTR_NEW(intarr(self.nhkl))
			self.usepeak = PTR_NEW(intarr(self.nhkl))
			OBJ_DESTROY, fitdata
			return, self.nhkl
		endif
	endelse
	j += 1
endwhile
MESSAGE, "I can not file any of the " + self.base + " FIT files..."
return, 0
end

; Get information of peaks
; returns array[4,npeaks] with
;    - array[0,i]: use peak i (0/1)
;    - array[1,i]: h for peak i
;    - array[2,i]: k for peak i
;    - array[3,i]: l for peak i
function experimentObject::getHKLInfo 
hklInfo = intarr(4,self.nhkl)
for i=0, self.nhkl-1 do begin
	hklInfo(0,i) = (*self.usepeak)(i)
	hklInfo(1,i) = (*self.h)(i)
	hklInfo(2,i) = (*self.k)(i)
	hklInfo(3,i) = (*self.l)(i)
endfor
return, hklInfo
end

; Set information for peaks
; send array[4,npeaks] with
;    - array[0,i]: use peak i (0/1)
;    - array[1,i]: h for peak i
;    - array[2,i]: k for peak i
;    - array[3,i]: l for peak i
pro experimentObject::setHKLInfo, hklInfo
for i=0, self.nhkl-1 do begin
	if (hklInfo(0,i) gt 0) then (*self.usepeak)(i) = 1 else (*self.usepeak)(i) = 0
	(*self.h)(i) = hklInfo(1,i)
	(*self.k)(i) = hklInfo(2,i)
	(*self.l)(i) = hklInfo(3,i)
endfor
self.peaksSet = 1
self->resetFit
end

; usedpeakindex
; peak indexing function, returns the real index of a peak indexed
; as used peak number (see explanations a few lines above)
function experimentObject::usedpeakindex, index
n = 0
for i=0, self.nhkl-1 do begin
	if ((*self.usepeak)[i] eq 1) then begin
		if (n eq index) then return, i
		n+=1
	endif
endfor
return, i
end

; Get peak name for peak i;
; returns a string with 'hkl'
function experimentObject::getPeakName, i, used = used
if ((i gt self.nhkl-1) or (i lt 0)) then return, ''
if KEYWORD_SET(used) then usei=self->usedpeakindex(i) else usei = i
return, strtrim(string((*self.h)(usei)),2) + strtrim(string((*self.k)(usei)),2) + strtrim(string((*self.l)(usei)),2)
end

; Get peaks names
; Returns an array of strings with
;    hklInfo[i] = hkl for peak i
function experimentObject::getPeakList, used = used
if KEYWORD_SET(used) then begin
	n = 0
	for i=0, self.nhkl-1 do if ((*self.usepeak)[i] eq 1) then n+=1
	hklInfo = strarr(n)
	j = 0
	for i=0, self.nhkl-1 do begin
		if ((*self.usepeak)[i] eq 1) then begin
			hklInfo[j] = strtrim(string((*self.h)(i)),2) + strtrim(string((*self.k)(i)),2) + strtrim(string((*self.l)(i)),2)
			j+=1
		endif
	endfor
	return, hklInfo
endif
hklInfo = strarr(self.nhkl)
for i=0, self.nhkl-1 do hklInfo[i] = strtrim(string((*self.h)(i)),2) + strtrim(string((*self.k)(i)),2) + strtrim(string((*self.l)(i)),2)
return, hklInfo
end

; Get h miller index
; returns h for peak i
function experimentObject::getH, i, used = used
if ((i gt self.nhkl-1) or (i lt 0)) then return, 0
if KEYWORD_SET(used) then usei=self->usedpeakindex(i) else usei = i
return, (*self.h)(usei)
end

; Get k miller index
; returns k for peak i
function experimentObject::getK, i, used = used
if ((i gt self.nhkl-1) or (i lt 0)) then return, 0
if KEYWORD_SET(used) then usei=self->usedpeakindex(i) else usei = i
return, (*self.k)(usei)
end

; Get l miller index
; returns l for peak i
function experimentObject::getL, i, used = used
if ((i gt self.nhkl-1) or (i lt 0)) then return, 0
if KEYWORD_SET(used) then usei=self->usedpeakindex(i) else usei=i
return, (*self.l)(usei)
end


; ****************************************** Lattice Strains Refinements *****************************

; returns experimental data for file index number index
function experimentObject::getExperimentalData, index
; print, 'Want data for number ', index
if ((*self.expdataset)[index] eq 0) then begin
	(*self.expdata)[index] = OBJ_NEW('FitPatternObject')
	(*self.expdataset)[index] = 1
	fileindex = intformat(index,self.digits)
	filename = strtrim(self.directory) + self->getFileName(index) + ".fit"
	; print, 'reading data from ' + filename
	if (FILE_TEST(filename) eq 1) then begin
		openr, lun, filename, /get_lun
		a = ((*self.expdata)[index])->readFromAscii(lun)
		free_lun, lun
	endif
	;print, 'Read from ascii, number of peaks: ', fitdata->getNPeaks();
	return, ((*self.expdata)[index])
endif
return, ((*self.expdata)[index])
end

; returns experimental data for an azimuth index
function experimentObject::getIntensityAtAngle, angle, peak
nPattern = N_ELEMENTS(*self.filelist)
; print, 'Want data for angle ', index
intensity = fltarr(nPattern)
for i=0, (nPattern-1) do begin
	if ((*self.expdataset)[i] eq 0) then begin
		(*self.expdata)[i] = OBJ_NEW('FitPatternObject')
		(*self.expdataset)[i] = 1
		fileindex = intformat(i,self.digits)
		filename = strtrim(self.directory) + self->getFileName(i) + ".fit"
		; print, 'reading data from ' + filename
		if (FILE_TEST(filename) eq 1) then begin
			openr, lun, filename, /get_lun
			a = ((*self.expdata)[i])->readFromAscii(lun)
			free_lun, lun
		endif
	;print, 'Read from ascii, number of peaks: ', fitdata->getNPeaks();
		; return, ((*self.expdata)[i])
	endif
	if ((*self.expdataset)[i] ne 0) then begin
		; read intensity for the peak and the angle chosen
		tt = (self->getExperimentalData(i))->getIntensityAtAngle(peak,angle)
		intensity[i]=tt
	endif
endfor
return, intensity
end

; Returns half widths of peaks.
; Added 05/06/2013 N Hilairet
function experimentObject::getPeakWidthAtAngle, angle, peak
  nPattern = N_ELEMENTS(*self.filelist)
  ; print, 'Want data for angle ', index
  peakwidths = fltarr(nPattern)
  for i=0, (nPattern-1) do begin
    if ((*self.expdataset)[i] eq 0) then begin
      (*self.expdata)[i] = OBJ_NEW('FitPatternObject')
      (*self.expdataset)[i] = 1
      fileindex = intformat(i,self.digits)
      filename = strtrim(self.directory) + self->getFileName(i) + ".fit"
      ; print, 'reading data from ' + filename
      if (FILE_TEST(filename) eq 1) then begin
        openr, lun, filename, /get_lun
        a = ((*self.expdata)[i])->readFromAscii(lun)
        free_lun, lun
      endif
      ;print, 'Read from ascii, number of peaks: ', fitdata->getNPeaks();
      ; return, ((*self.expdata)[i])
    endif
    if ((*self.expdataset)[i] ne 0) then begin
      ; read width for the peak and the angle chosen
      tt = (self->getExperimentalData(i))->getPeakWidthAtAngle(peak,angle)
      peakwidths[i]=tt
    endif
  endfor
  return,  peakwidths
end

function experimentObject::latticeStrainFileIndex, index
return, self->latticeStrain((*self.fileindex)[index])
end

; refine lattice strains equations for file number index
function experimentObject::latticeStrain, index
; if it has not been refined yet, we do it...
if ((*self.fitstrainsset)[index] eq 0) then begin
	(*self.fitstrains)(index) = OBJ_NEW('latticeStrainObject')
	(*self.fitstrainsset)[index] = 1
	if ((self.peaksSet eq 0) or (self.materialSet eq 0)) then return, (*(self.fitstrains))[index]
	; print, 'Get data for ', index
	fitdata = self->getExperimentalData(index)
	d0 = fltarr(self.nhkl)
	dd0 = fltarr(self.nhkl)
	Q0 = fltarr(self.nhkl)
	dQ0 = fltarr(self.nhkl)
	nfit = 0
	peaks = where( (*self.usepeak) eq 1)
	if (self.fitcenter eq 1) then begin
		test = fitdata->latticeStrainFitCenter(peaks,self.wavelength,self.offset)
		((*(self.fitstrains))[index])->setFitResultsWithCenter, peaks, (*self.h), (*self.k), (*self.l), self.offset, test
	endif else if (self.fitoffset eq 1) then begin
		test = fitdata->latticeStrainFitOffset(peaks,self.wavelength,self.offset)
		((*(self.fitstrains))[index])->setFitResultsWithOffset, peaks, (*self.h), (*self.k), (*self.l), test
	endif else begin
		test = fitdata->latticeStrainFitNoCorr(peaks,self.wavelength,self.offset)
		((*(self.fitstrains))[index])->setFitResultsNoCorr, peaks, (*self.h), (*self.k), (*self.l), self.offset, test
	endelse
	;
	;for i=0, self.nhkl-1 do begin
	;	if ((*self.usepeak)[i] eq 1) then begin
	;		dd = fitdata->latticeStrainFit(i,self.wavelength)
	;		d0[i] = dd[0]
	;		dd0[i] = dd[1]
	;		Q0[i] = dd[2]
	;		dQ0[i] = dd[3]
	;		nfit +=1
	;	endif
	;endfor
	;((*(self.fitstrains))[index])->setHKLDQ, self.nhkl, nfit, (*self.usepeak), (*self.h), (*self.k), (*self.l), d0, dd0, Q0, dQ0
	return, (*(self.fitstrains))[index]
endif
; if it has been done already, we return the previous resuls...
;print, 'No need to recaculate, n peaks: ', ((*(self.fitstrains))[index])->getnhkl()
return, (*(self.fitstrains))[index]
end

; latticeStrainD0
; returns D0 for peak number peakindex at file number index
; index have to be used index (see explanations above) since the 
; latticeStrainObjects are indexed that way
function experimentObject::latticeStrainD0, index, peakindex, used=used
if ((self.peaksSet eq 0) or (self.materialSet eq 0)) then return, 0.
fit = self->latticeStrain(index)
; print, peakindex, fit->getD()
if (fit->getSet() eq 1) then d0 = (fit->getD())[peakindex] else d0 = 0.
return, d0
end

; latticeStrainErrD0
; returns D0 for peak number peakindex at file number index
; index have to be used index (see explanations above) since the 
; latticeStrainObjects are indexed that way
function experimentObject::latticeStrainErrD0, index, peakindex, used=used
if ((self.peaksSet eq 0) or (self.materialSet eq 0)) then return, 0.
fit = self->latticeStrain(index)
; print, peakindex, fit->getD()
if (fit->getSet() eq 1) then d0 = (fit->getdD())[peakindex] else d0 = 0.
return, d0
end

; latticeStrainQ
; returns Q for peak number peakindex at file number index
; index have to be used index (see explanations above) since the 
; latticeStrainObjects are indexed that way
function experimentObject::latticeStrainQ, index, peakindex, used=used
if ((self.peaksSet eq 0) or (self.materialSet eq 0)) then return, 0.
fit = self->latticeStrain(index)
; print, peakindex, fit->getD()
if (fit->getSet() eq 1) then Q = (fit->getQ())[peakindex] else Q = 0.
return, Q
end

; latticeStrainErrQ
; returns error on Q for peak number peakindex at file number index
; index have to be used index (see explanations above) since the 
; latticeStrainObjects are indexed that way
function experimentObject::latticeStrainErrQ, index, peakindex, used=used
if ((self.peaksSet eq 0) or (self.materialSet eq 0)) then return, 0.
fit = self->latticeStrain(index)
; print, peakindex, fit->getD()
if (fit->getSet() eq 1) then dQ = (fit->getdQ())[peakindex] else dQ = 0.
return, dQ
end


; latticeStrainBeamCenter
; returns beam center fitted for file number index
; xx[0] = x-center
; xx[1] = y-center
function experimentObject::latticeStrainBeamCenter, index
if ((self.peaksSet eq 0) or (self.materialSet eq 0)) then return, [0.,0.]
fit = self->latticeStrain(index)
if (fit->getSet() eq 1) then return, [fit->getCenterX(), fit->getCenterY()]
return, [0.,0.]
end

; latticeStrainBeamCenterX
; returns beam x-center fitted for file number index
function experimentObject::latticeStrainBeamCenterX, index
if ((self.peaksSet eq 0) or (self.materialSet eq 0)) then return, [0.,0.]
fit = self->latticeStrain(index)
if (fit->getSet() eq 1) then return, fit->getCenterX()
return, [0.,0.]
end

; latticeStrainBeamCenterY
; returns beam y-center fitted for file number index
function experimentObject::latticeStrainBeamCenterY, index
if ((self.peaksSet eq 0) or (self.materialSet eq 0)) then return, [0.,0.]
fit = self->latticeStrain(index)
if (fit->getSet() eq 1) then return, fit->getCenterY()
return, [0.,0.]
end

; latticeStrainBeamCenterErrX
; returns the error on beam x-center fitted for file number index
function experimentObject::latticeStrainBeamCenterErrX, index
if ((self.peaksSet eq 0) or (self.materialSet eq 0)) then return, [0.,0.]
fit = self->latticeStrain(index)
if (fit->getSet() eq 1) then return, fit->getCenterErrX()
return, [0.,0.]
end

; latticeStrainBeamCenterErrY
; returns the error on beam y-center fitted for file number index
function experimentObject::latticeStrainBeamCenterErrY, index
if ((self.peaksSet eq 0) or (self.materialSet eq 0)) then return, [0.,0.]
fit = self->latticeStrain(index)
if (fit->getSet() eq 1) then return, fit->getCenterErrY()
return, [0.,0.]
end


; latticeStrainOffset
; returns offset fitted (or imposed) for file number index
function experimentObject::latticeStrainOffset, index
if ((self.peaksSet eq 0) or (self.materialSet eq 0)) then return, [0.]
fit = self->latticeStrain(index)
if (fit->getSet() eq 1) then return, fit->getOffset()
return, [0.]
end

; latticeStrainErrOffset
; returns error on offset fitted (or imposed) for file number index
function experimentObject::latticeStrainErrOffset, index
if ((self.peaksSet eq 0) or (self.materialSet eq 0)) then return, [0.]
fit = self->latticeStrain(index)
if (fit->getSet() eq 1) then return, fit->getErrOffset()
return, [0.]
end


; refineUnitCell
; parameter:
;   - index: file index
; returns: cell object
; returns a unit cell object with the unit cell parameters fitted to the D0 
; obtained in the lattice strains fits
function experimentObject::refineUnitCell, index
fit = self->latticeStrain(index)
cell = (*self.material)->refineUnitCell(fit)
return, cell
end

; refineUnitCell
; parameter:
;   - index: file index
; returns: cell object
; returns a unit cell object with the unit cell parameters fitted to the D0
; obtained in the lattice strains fits, and pressure refined using the unit cell
; parameters and equation of state
function experimentObject::refinePressure, index
fit = self->latticeStrain(index)
cell = (*self.material)->refinePressure(fit)
return, cell
end

; refineVolume
; parameter:
;   - index: file index
; returns: cell object
; returns a unit cell object with the unit cell parameters fitted to the D0
; obtained in the lattice strains fits, and volumes calculated using the unit cell
; parameters
function experimentObject::refineVolume, index
fit = self->latticeStrain(index)
cell = (*self.material)->refineVolume(fit)
return, cell
end

function experimentObject::summaryQ, step
if (self.materialSet eq 0) then return, "Not set\n"
cell = self->refinePressure(step)
p = cell->getPressure()
dp = cell->getErrPressure()
txt = cell->getName()
txt += (*self.filelist)[step]
txt += "\nP = " + fltformatA(p) + " (+/-) "+ fltformatA(dp) + "\n"
OBJ_DESTROY, cell
values = self->getPeakList(/used)
for i=0, n_elements(values)-1 do begin
	txt += '   Q(' + values[i] + ') = ' + fltformatA(self->latticeStrainQ(step, i, /used)) + ' (+/-) ' + fltformatA(self->latticeStrainErrQ(step, i, /used)) + '\n'
endfor
return, txt + '\n'
end

function experimentObject::summaryQnoP, step
if (self.materialSet eq 0) then return, "Not set\n"
txt = (*self.filelist)[step] + '\n'
values = self->getPeakList(/used)
for i=0, n_elements(values)-1 do begin
	txt += '   Q(' + values[i] + ') = ' + fltformatA(self->latticeStrainQ(step, i, /used)) + ' (+/-) ' + fltformatA(self->latticeStrainErrQ(step, i, /used)) + '\n'
endfor
return, txt + '\n'
end

function experimentObject::summaryBeamCenter, step
if (self.materialSet eq 0) then return, "Not set\n"
txt = (*self.filelist)[step] + '\n'
txt += '    X-center = ' + fltformatC(self->latticeStrainBeamCenterX(step)) + ' (+/-) ' + fltformatC(self->latticeStrainBeamCenterErrX(step)) + '\n'
txt += '    Y-center = ' + fltformatC(self->latticeStrainBeamCenterY(step)) + ' (+/-) ' + fltformatC(self->latticeStrainBeamCenterErrY(step)) + '\n'
return, txt + '\n'
end

function experimentObject::summaryOffset, step
if (self.materialSet eq 0) then return, "Not set\n"
txt = (*self.filelist)[step] + '\n'
txt += '    offset = ' + fltformatC(self->latticeStrainOffset(step)) + ' (+/-) ' + fltformatC(self->latticeStrainErrOffset(step)) + '\n'
return, txt + '\n'
end

; *************************************************** stresses ********************


function experimentObject::refineTHKL, index, peakindex, used=used
if ((self.peaksSet eq 0) or (self.materialSet eq 0)) then return, 0.
fit = self->latticeStrain(index)
cell = (*self.material)->refinePressure(fit)
if (fit->getSet() ne 1) then return, 0.
hh = self->getH(peakindex, /used)
kk = self->getK(peakindex, /used)
ll = self->getL(peakindex, /used)
Q = (fit->getQ())[peakindex]
TwoG = (*self.material)->getTwoG(hh, kk, ll, cell)
t = 3.*Q*TwoG
; print, hh, kk, ll, Q, TwoG, t
return, t
OBJ_DESTROY, cell
end


function experimentObject::summaryTHKL, index
if (self.materialSet eq 0) then return, "Not set\n"
fit = self->latticeStrain(index)
cell = (*self.material)->refinePressure(fit)
txt = cell->getName()
txt += (*self.filelist)[index]
p = cell->getPressure()
dp = cell->getErrPressure()
txt += "\nP = " + fltformatA(p) + " (+/-) "+ fltformatA(dp) + "\n"
peaks = self->getPeakList(/used)
t = fltarr(n_elements(peaks))
dt = fltarr(n_elements(peaks))
for peakindex=0, n_elements(peaks)-1 do begin
	if (fit->getSet() eq 1) then Q = (fit->getQ())[peakindex] else Q = 0.
	if (fit->getSet() eq 1) then dQ = (fit->getdQ())[peakindex] else dQ = 0.
	hh = self->getH(peakindex, /used)
	kk = self->getK(peakindex, /used)
	ll = self->getL(peakindex, /used)
	g = (*self.material)->getTwoG(hh, kk, ll, cell)
	dg = (*self.material)->getErrTwoG(hh, kk, ll, cell)
	t[peakindex] = 3.*Q*g
	dt[peakindex] = sqrt( (3.*Q*dg)^2 + (3.*g*dQ)^2)
	;print, "hkl Q, dQ, g, dG, t, dt", hh, kk, ll, Q, dQ, g, dg, t[peakindex], dt[peakindex]
	txt += '   t(' + peaks[peakindex] + ') = ' + fltformatA(t[peakindex]) + ' (+/-) ' +  fltformatA(dt[peakindex]) + '\n'
endfor
OBJ_DESTROY, cell
return, txt + '\n'
end


; ***************************************************  cell parameter stuff ********

; returns an array of string with the name of the unit cell parameters
; e.g. a for cubic, a and c for hexagonal...
function experimentObject::getCellParList
return, (*self.material)->getCellParList()
end

; returns the name of the unit cell parameters number i
; e.g. i=0, a for cubic, i=0, a and i=1, c for hexagonal...
function experimentObject::getCellParName, i
return, (*self.material)->getCellParName(i)
end


; refineCellPar
; parameter:
;   - index: file index
;   - selected: index of the unit cell parameter
; returns: the value of the unit cell parameter
; obtained in the lattice strains fits
function experimentObject::refineCellPar, index, selected
fit = self->latticeStrain(index)
cell = (*self.material)->refineUnitCell(fit)
return, cell->getCellParValue(selected)
end


; *************************************************** Text summaries and informations  ********

; function getFirst
; returns
;   - first index for file numbers
function experimentObject::getFirst
return, self.first
end

; function getLast
; returns
;   - last index for file numbers
function experimentObject::getLast
return, self.last
end

; function getBase
; returns
;   - base name for file names
function experimentObject::getBase
return, self.base
end

; function getDirectory
; returns
;   - directory were data is stored
function experimentObject::getDirectory
return, self.directory
end

; function getDigits
; returns
;   - number of digits for defining file names
function experimentObject::getDigits
return, self.digits
end

; function infoHKLLine
; returns
;   - list of hkl planes, on one line
function experimentObject::infoHKLLine
if (self.peaksSet eq 0) then return, 'Not set'
str = ""
for i=0, self.nhkl-1 do begin
	if ((*self.usepeak)(i) eq 1) then str += STRTRIM(STRING((*self.h)(i),/PRINT),2) + STRTRIM(STRING((*self.k)(i),/PRINT),2) + STRTRIM(STRING((*self.l)(i),/PRINT),2) + " " else str += "??? "
endfor
return, str
end

; function infoHKLTxt
; returns
;   - verbose list of hkl planes
function experimentObject::infoHKLTxt
if (self.peaksSet eq 0) then return, 'Lattice planes information not set'
str = 'Lattice planes information:\n'
for i=0, self.nhkl-1 do begin
	str += '\tPeak ' + STRTRIM(STRING(i+1,/PRINT),2) + ': ' 
	if ((*self.usepeak)(i) eq 0) then str += 'not used.\n' else str += STRTRIM(STRING((*self.h)(i),/PRINT),2) + STRTRIM(STRING((*self.k)(i),/PRINT),2) + STRTRIM(STRING((*self.l)(i),/PRINT),2) + ' \n'
endfor
return, str
end

; function infoFITTxt
; returns
;   - verbose information on data files
function experimentObject::infoFITTxt
str = "FIT files information:\n"
str += "\tDirectory with FIT files: " + self.directory + "\n"
str += "\tBase name for FIT files: " + self.base + "\n"
str += "\tFirst index: " + STRTRIM(STRING(self.first,/PRINT),2) + "\n"
str += "\tLast index: " + STRTRIM(STRING(self.last,/PRINT),2) + "\n"
str += "\tNumber of digits: " + STRTRIM(STRING(self.digits,/PRINT),2) + "\n"
return, str
end

; function infoWaveTxt
; returns
;   - wavelength information
function experimentObject::infoWaveTxt
str = "Wavelength:" + STRTRIM(STRING(self.wavelength,/PRINT),2) + "\n"
return, str
end

; function infoTxt
; returns
;   - information about the experiment, all of it
function experimentObject::infoTxt
str = "Information about this experiment:\n"
str += "Wavelength:" + STRTRIM(STRING(self.wavelength,/PRINT),2) + "\n"
str += self->infoFITTxt()
str += self->infoHKLTxt()
str += self->infoMaterialTxt()
return, str
end

; function infoMaterialLine
; returns
;   - material name
function experimentObject::infoMaterialLine
if (self.materialSet eq 0) then return, "Not set"
str = (*self.material)->getName()
return, str
end

; function infoMaterialTxt
; returns
;   - all information on the material (name, symmetry, elastic properties...)
function experimentObject::infoMaterialTxt
if (self.materialSet eq 0) then return, "Material poperties not set\n"
str = (*self.material)->infoTxt()
return, str
end

; ***************************************************************** CVS export functions  ********

; refineAllPressuresCVS
; parameter:
;   - progressBar: widget ID of a progress bar object
; returns:
;   - string with CVS results
; Each line includes
;   - file number
;   - unit cell parameters fitted and errors
;   - volumes calculated from unit cell parameters and errors
;   - pressures calculated from unit cell parameters and errors
function experimentObject::refineAllPressuresCVS, progressBar
txt = (*self.material)->labelPCSV() + "\n"
n = self->getNFitFiles()
for i=0,n-1 do begin
	fit = self->latticeStrain(i)
	if (fit->getSet() eq 1) then begin
		cell = (*self.material)->refineUnitCell(fit)
		cell = self->refinePressure(i)
		txt += STRING((*self.fileindex)[i]) + STRING(9B) + cell->summaryPCSV() + "\n"
	endif
	percent = 100.*(i)/n
	progressBar->Update, percent
endfor
return, txt
end


function experimentObject::summaryQCSV, step
if (self.materialSet eq 0) then return, "Not set\n"
cell = self->refinePressure(step)
p = cell->getPressure()
dp = cell->getErrPressure()
txt = STRING((*self.fileindex)[step]) + STRING(9B) + fltformatA(p) + STRING(9B) + fltformatA(dp)
OBJ_DESTROY, cell
values = self->getPeakList(/used)
for i=0, n_elements(values)-1 do begin
	txt += STRING(9B) + fltformatA(self->latticeStrainQ(step, i, /used))  + STRING(9B) + fltformatA(self->latticeStrainErrQ(step, i, /used))
endfor
return, txt
end

function experimentObject::summaryQCSVnoP, step
if (self.materialSet eq 0) then return, "Not set\n"
txt = STRING((*self.fileindex)[step])
values = self->getPeakList(/used)
for i=0, n_elements(values)-1 do begin
	txt += STRING(9B) + fltformatA(self->latticeStrainQ(step, i, /used))  + STRING(9B) + fltformatA(self->latticeStrainErrQ(step, i, /used))
endfor
return, txt
end

function experimentObject::summaryQCSVAll, progressbar
if (self.materialSet eq 0) then return, "Not set\n"
values = self->getPeakList(/used)
txt = "#" + STRING(9B) + "P" + STRING(9B) + "dP"
for i=0, n_elements(values)-1 do txt += STRING(9B) + "Q(" + values[i] + ")" + STRING(9B) + "err"
txt += "\n"
n = self->getNFitFiles()
for step=0,n-1 do begin
	txt += self->summaryQCSV(step) + '\n'
	percent = 100.*step/n
	progressBar->Update, percent
endfor
return, txt
end

function experimentObject::summaryQCSVAllnoP, progressbar
if (self.materialSet eq 0) then return, "Not set\n"
values = self->getPeakList(/used)
txt='#'
for i=0, n_elements(values)-1 do txt += STRING(9B) + "Q(" + values[i] + ")" + STRING(9B) + "err"
txt += "\n"
n = self->getNFitFiles()
for step=0,n-1 do begin
	txt += self->summaryQCSVnoP(step) + '\n'
	percent = 100.*step/n
	progressBar->Update, percent
endfor
return, txt
end

function experimentObject::summaryBeamCenterCSVAll, progressbar
if (self.materialSet eq 0) then return, "Not set\n"
txt = "#" + STRING(9B) + "X-center" + STRING(9B) + "Err X-center" + STRING(9B) + "Y-center" + STRING(9B) + "Err Y-center" + '\n'
n = self->getNFitFiles()
for step=0,n-1 do begin
	txt += STRING((*self.fileindex)[step]) + STRING(9B) + fltformatC(self->latticeStrainBeamCenterX(step)) + STRING(9B) +  fltformatC(self->latticeStrainBeamCenterErrX(step)) + STRING(9B) + fltformatC(self->latticeStrainBeamCenterY(step)) + STRING(9B) + fltformatC(self->latticeStrainBeamCenterErrY(step)) + '\n'
endfor
return, txt
end


function experimentObject::summaryOffsetCSVAll, progressbar
if (self.materialSet eq 0) then return, "Not set\n"
txt = "#" + STRING(9B) + "Offset" + STRING(9B) + "Err Offset" + '\n'
n = self->getNFitFiles()
for step=0,n-1 do begin
	txt += STRING((*self.fileindex)[step]) + STRING(9B) + fltformatC(self->latticeStrainOffset(step)) + STRING(9B) +  fltformatC(self->latticeStrainErrOffset(step)) + '\n'
endfor
return, txt
end

function experimentObject::summaryTCSV, step
if (self.materialSet eq 0) then return, "Not set\n"
cell = self->refinePressure(step)
p = cell->getPressure()
dp = cell->getErrPressure()
txt = STRING((*self.fileindex)[step]) + STRING(9B) + fltformatA(p) + STRING(9B) + fltformatA(dp)
values = self->getPeakList(/used)
for i=0, n_elements(values)-1 do begin
	hh = self->getH(i, /used)
	kk = self->getK(i, /used)
	ll = self->getL(i, /used)
	Q = self->latticeStrainQ(step, i, /used)
	dQ = self->latticeStrainErrQ(step, i, /used)
	g = (*self.material)->getTwoG(hh, kk, ll, cell)
	dg = (*self.material)->getErrTwoG(hh, kk, ll, cell)
	t = 3.*Q*g
	dt = sqrt( (3.*Q*dg)^2 + (3.*g*dQ)^2)
	txt += STRING(9B) + fltformatA(t)  + STRING(9B) + fltformatA(dt)
endfor
OBJ_DESTROY, cell
return, txt
end

function experimentObject::summaryTCSVAll, progressbar
if (self.materialSet eq 0) then return, "Not set\n"
values = self->getPeakList(/used)
txt = "#" + STRING(9B) + "P" + STRING(9B) + "dP"
for i=0, n_elements(values)-1 do txt += STRING(9B) + "t(" + values[i] + ")" + STRING(9B) + "err"
txt += "\n"
n = self->getNFitFiles()
for step=0,n-1 do begin
	txt += self->summaryTCSV(step) + '\n'
	percent = 100.*i/n
	progressBar->Update, percent
endfor
return, txt
end

function experimentObject::summaryUnitCellCSV, step
if (self.materialSet eq 0) then return, "Not set\n"
cell = self->refineUnitCell(step)
txt = STRING((*self.fileindex)[step]) + " "
peaks = self->getPeakList(/used)
for i=0, n_elements(peaks)-1 do begin
	hh = self->getH(i, /used)
	kk = self->getK(i, /used)
	ll = self->getL(i, /used)
	txt += STRING(9B) + fltformatA(self->latticeStrainD0(step, i, /used)) + STRING(9B) + fltformatA(self->latticeStrainErrD0(step, i, /used)) + STRING(9B) + fltformatA(cell->getDHKL(hh,kk,ll))
endfor
cellpar = (*self.material)->getCellParList()
for i=0, n_elements(cellpar)-1 do begin
	txt += STRING(9B) + fltformatA(cell->getCellParValue(i)) + STRING(9B) + fltformatA(cell->getCellErrParValue(i))
endfor
OBJ_DESTROY, cell
return, txt
end

function experimentObject::summaryUnitCellCSVAll, progressbar
if (self.materialSet eq 0) then return, "Not set\n"
peaks = self->getPeakList(/used)
txt = "#"
for i=0, n_elements(peaks)-1 do txt += STRING(9B) + "dm0(" + peaks[i] + ")" + STRING(9B) +"err"+ STRING(9B) + " dr0(" + peaks[i] + ")"
cellpar = (*self.material)->getCellParList()
for i=0, n_elements(cellpar)-1 do txt += STRING(9B)  + cellpar[i] + STRING(9B) + "err"
txt += "\n"
n = self->getNFitFiles()
for step=0,n-1 do begin
	txt += '' + self->summaryUnitCellCSV(step) + '\n'
	percent = 100.*step/n
	progressBar->Update, percent
endfor
return, txt
end



; *************************************************** ASCII Import and Export ****************

function experimentObject::saveToAscii, lun
printf, lun, '# Experiment analysis file'
printf, lun, '# File version'
printf, lun, '2'
printf, lun, '# Directory with FIT files'
printf, lun, self.directory
printf, lun, '# Basename for FIT files'
printf, lun, self.base
printf, lun, '# First index for FIT files'
printf, lun, STRING(self.first, /PRINT)
printf, lun, '# Last index for FIT files'
printf, lun, STRING(self.last, /PRINT)
printf, lun, '# Number of digits for FIT files'
printf, lun, STRING(self.digits, /PRINT)
printf, lun, '# Wavelength'
printf, lun, STRING(self.wavelength, /PRINT)
printf, lun, '# Fit offset for maximum stress 1 for yes, 0 for no'
printf, lun, STRING(self.fitoffset, /PRINT)
printf, lun, '# Starting offset value, in degrees'
printf, lun, STRING(self.offset, /PRINT)
printf, lun, '# Fit beam center; 1 for yes, 0 for no'
printf, lun, STRING(self.fitcenter, /PRINT)
printf, lun, '# Material properties set (1/0)'
printf, lun, STRING(self.materialSet, /PRINT)
printf, lun, '# Peaks properties set (1/0)'
printf, lun, STRING(self.peaksSet, /PRINT)
if (self.peaksSet eq 1) then begin
	printf, lun, '# Number of peaks'
	printf, lun, STRING(self.nhkl, /PRINT)
	printf, lun, '# Peaks info (use, h, k, l)'
	for i=0, self.nhkl-1 do begin
		printf, lun, STRING((*self.usepeak)(i), /PRINT) + STRING((*self.h)(i), /PRINT) + STRING((*self.k)(i), /PRINT) + STRING((*self.l)(i), /PRINT)
	endfor
endif
if (self.materialSet eq 1) then begin
	noerror = (*self.material)->saveToAscii(lun)
	if (noerror ne 1) then return, noerror
endif
RETURN, 1
end

function experimentObject::readFromAsciiV1, lun
on_ioerror, bad
; Directory
row = readascii(lun,  com="#")
self.directory = STRTRIM(row,2)
; Basename for FIT files
row = readascii(lun,  com="#")
self.base = STRTRIM(row,2)
; First index for FIT files
self.first = fix(readascii(lun,  com="#"))
; Last index for FIT files
self.last = fix(readascii(lun,  com="#"))
; Number of digits for FIT files
self.digits = fix(readascii(lun,  com="#"))
; wavelength
self.wavelength = float(readascii(lun,  com="#"))
; Material properties set?
self.materialSet = fix(readascii(lun,  com="#"))
; Peaks properties set?
self.peaksSet = fix(readascii(lun,  com="#"))
if (self.peaksSet eq 1) then begin
	row = readascii(lun,  com="#")
	self.nhkl = fix(row)
	self.h = PTR_NEW(intarr(self.nhkl))
	self.k = PTR_NEW(intarr(self.nhkl))
	self.l = PTR_NEW(intarr(self.nhkl))
	self.usepeak = PTR_NEW(intarr(self.nhkl))
	for i=0, self.nhkl-1 do begin
		row = strsplit(readascii(lun,  com='#'), /extract)
		(*self.usepeak)(i) = fix(row[0])
		(*self.h)(i) = fix(row[1])
		(*self.k)(i) = fix(row[2])
		(*self.l)(i) = fix(row[3])
	endfor
endif
; Material properties set?
if (self.materialSet eq 1) then begin
	noerror = (*self.material)->readFromAscii(lun)
	if (noerror ne 1) then return, noerror
endif
self.fitstrains = PTR_NEW(OBJARR(self.last+1))
self.fitstrainsset = PTR_NEW(intarr(self.last+1));
(*self.fitstrainsset)[*] = 0
self.expdata = PTR_NEW(OBJARR(self.last+1))
self.expdataset = PTR_NEW(intarr(self.last+1));
(*self.expdataset)[*] = 0
return, self->setFileList()
RETURN, 1
bad: return, !ERR_STRING
end

function experimentObject::readFromAsciiV2, lun
on_ioerror, bad
; Directory
row = readascii(lun,  com="#")
self.directory = STRTRIM(row,2)
; Basename for FIT files
row = readascii(lun,  com="#")
self.base = STRTRIM(row,2)
; First index for FIT files
self.first = fix(readascii(lun,  com="#"))
; Last index for FIT files
self.last = fix(readascii(lun,  com="#"))
; Number of digits for FIT files
self.digits = fix(readascii(lun,  com="#"))
; wavelength
self.wavelength = float(readascii(lun,  com="#"))
; fit offset?
self.fitoffset = fix(readascii(lun,  com="#"))
; offset value
self.offset = float(readascii(lun,  com="#"))
; fit center?
self.fitcenter = fix(readascii(lun,  com="#"))
; Material properties set?
self.materialSet = fix(readascii(lun,  com="#"))
; Peaks properties set?
self.peaksSet = fix(readascii(lun,  com="#"))
if (self.peaksSet eq 1) then begin
	row = readascii(lun,  com="#")
	self.nhkl = fix(row)
	self.h = PTR_NEW(intarr(self.nhkl))
	self.k = PTR_NEW(intarr(self.nhkl))
	self.l = PTR_NEW(intarr(self.nhkl))
	self.usepeak = PTR_NEW(intarr(self.nhkl))
	for i=0, self.nhkl-1 do begin
		row = strsplit(readascii(lun,  com='#'), /extract)
		(*self.usepeak)(i) = fix(row[0])
		(*self.h)(i) = fix(row[1])
		(*self.k)(i) = fix(row[2])
		(*self.l)(i) = fix(row[3])
	endfor
endif
; Material properties set?
if (self.materialSet eq 1) then begin
	noerror = (*self.material)->readFromAscii(lun)
	if (noerror ne 1) then return, noerror
endif
self.fitstrains = PTR_NEW(OBJARR(self.last+1))
self.fitstrainsset = PTR_NEW(intarr(self.last+1));
(*self.fitstrainsset)[*] = 0
self.expdata = PTR_NEW(OBJARR(self.last+1))
self.expdataset = PTR_NEW(intarr(self.last+1));
(*self.expdataset)[*] = 0
return, self->setFileList()
RETURN, 1
bad: return, !ERR_STRING
end

function experimentObject::readFromAscii, lun
on_ioerror, bad
if (self.last gt 0) then begin
	for i=0, self.last do $
		if ((*self.fitstrainsset)[i] eq 1) then OBJ_DESTROY, ((*self.fitstrains)[i])
	for i=0, self.last do $
		if ((*self.expdataset)[i] eq 1) then OBJ_DESTROY, ((*self.expdata)[i])
	PTR_FREE, self.fitstrains
	PTR_FREE, self.fitstrainsset
	PTR_FREE, self.expdata
	PTR_FREE, self.expdataset
endif
if (self.filelistset eq 1) then begin
	filelistset = 0
	PTR_FREE, self.filelist
	PTR_FREE, self.fileindex
endif
self.fitoffset = 0
self.offset = 0.0
self.fitcenter = 1
; file version
version = fix(readascii(lun, com="#"))
switch version OF
	1: return, self->readFromAsciiV1(lun)
	2: return, self->readFromAsciiV2(lun)
	else: return, 'Sorry, we can only read file format 1 or 2 at this time'
endswitch
RETURN, 1
bad: return, !ERR_STRING
end