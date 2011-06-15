COMPILE_OPT IDL2 

PRO FitPatternObject__DEFINE 
	struct = { FitPatternObject, nsubpat:0, peakprofile: 0, subpatterns: PTR_NEW() }
END

; basic init method
function FitPatternObject::Init
return, 1
end

; Cleanup method
pro FitPatternObject::Cleanup
end

; Use the fit results to build a synthetic dataset.
; send 
;       + the array of azimuth angles
;       + the array of 2theta
;       + the number of azimuth angles
;       + the number of 2theta values
; If the azimuth angles are different that those in the fit, it will fail!
function FitPatternObject::buildSynthethicData, alpha, twotheta, nalpha, ntheta
builtdata = fltarr(nalpha,ntheta)
for i=0, nalpha-1 do begin
	for j=0, self.nsubpat-1 do begin
		builtdata(i,*) = builtdata(i,*) + (*(self.subpatterns))(j)->syntheticdata(alpha(i),twotheta,ntheta)
	endfor
endfor
return, builtdata
end

; Lattice strain.. Fits a lattice strain equation for one peak with no center correction
function FitPatternObject::latticeStrainFit, i, wavelength
index = self->findPeak(i)
return, (*self.subpatterns)(index[0])->latticeStrainFit(index[1], wavelength)
end


; Lattice strain fit for several peaks, no offset fitting, no beam center correction
function FitPatternObject::latticeStrainFitNoCorr, peaks, wavelength, offsetstart
common fitinfo, npeaks, ndeltapeak, wave, offset
; Extracting data to fit
wave = wavelength
offset = !PI*offsetstart/180.
npeaks = N_ELEMENTS(peaks)
delta = PTRARR(npeaks)
mes2Theta = PTRARR(npeaks)
ndeltapeak = intarr(npeaks)
ndelta = 0
for i=0, npeaks-1 do begin
	index = self->findPeak(peaks[i])
	delta[i] = ptr_new((*self.subpatterns)(index[0])->getDelta(index[1]))
	mes2Theta[i] = ptr_new((*self.subpatterns)(index[0])->getTheta(index[1]))
	ndeltapeak[i] = N_ELEMENTS(*delta[i])
	ndelta += ndeltapeak[i]
endfor
; For each peak, we fit d0 and Q + 1 parameter for offset
; Starting values are
;   - d0: av(d)
;   - Q: 0
guess = fltarr(2*npeaks)
for i=1, npeaks do begin
	guess[2*(i-1)] = mean(wavelength/(2.*sin(!PI*(*mes2Theta[i-1])/360.))) ; d0 for peak i
	guess[2*(i-1)+1] = 0. ; Q for peak i
end
; creating x and y data to fit
x = fltarr(ndelta)
y = fltarr(ndelta)
step = 0
; In order to set the fit data, we simply put all 2theta vs. delta one peak after the other-1 do begin
for i=0, npeaks-1 do begin
	for j=0, ndeltapeak[i]-1 do begin
		x[step] = !PI*(*delta[i])[j]/180.
		y[step] = !PI*(*mes2Theta[i])[j]/180.
		;
		step += 1
	endfor
endfor
sigma = 0.0005*(y)
fit = MPFITFUN('LATTICESTRAINMULTIPLEPEAKSNOCORR', x, y, sigma, guess, perror = perror, /quiet)
;for i=1, npeaks do fit[2*i] = wavelength/(2.*sin(fit[2*i]))
return, {fit:fit , perror: perror}
end


; Lattice strain fit for several peaks WITH center correction
function FitPatternObject::latticeStrainFitCenter, peaks, wavelength, offsetstart
common fitinfo, npeaks, ndeltapeak, wave, offset
; Extracting data to fit
wave = wavelength
offset = !PI*offsetstart/180.
npeaks = N_ELEMENTS(peaks)
delta = PTRARR(npeaks)
mes2Theta = PTRARR(npeaks)
ndeltapeak = intarr(npeaks)
ndelta = 0
for i=0, npeaks-1 do begin
	index = self->findPeak(peaks[i])
	delta[i] = ptr_new((*self.subpatterns)(index[0])->getDelta(index[1]))
	mes2Theta[i] = ptr_new((*self.subpatterns)(index[0])->getTheta(index[1]))
	ndeltapeak[i] = N_ELEMENTS(*delta[i])
	ndelta += ndeltapeak[i]
endfor
; For each peak, we fit d0 and Q + 2 parameters for center correction (x0, y0)
; Starting values are
;   - d0: av(d)
;   - Q: 0
;   - x0: 0
;   - y0: 0
guess = fltarr(2*npeaks+2)
guess[0] = 0.001 ; x0
guess[1] = 0.001 ; y0
for i=1, npeaks do begin
	guess[2*i] = mean(wavelength/(2.*sin(!PI*(*mes2Theta[i-1])/360.))) ; d0 for peak i
	guess[2*i+1] = 0. ; Q for peak i
end
; creating x and y data to fit
x = fltarr(ndelta)
y = fltarr(ndelta)
step = 0
; In order to set the fit data, we simply put all 2theta vs. delta one peak after the other-1 do begin
for i=0, npeaks-1 do begin
	for j=0, ndeltapeak[i]-1 do begin
		x[step] = !PI*(*delta[i])[j]/180.
		y[step] = !PI*(*mes2Theta[i])[j]/180.
		;
		step += 1
	endfor
endfor
sigma = 0.0005*(y)
fit = MPFITFUN('LATTICESTRAINMULTIPLEPEAKSCENTER', x, y, sigma, guess, perror = perror, /quiet)
;for i=1, npeaks do fit[2*i] = wavelength/(2.*sin(fit[2*i]))
return, {fit:fit , perror: perror}
end


; Lattice strain fit for several peaks WITH offset correction
function FitPatternObject::latticeStrainFitOffset, peaks, wavelength, offsetstart
common fitinfo, npeaks, ndeltapeak, wave, offset
; Extracting data to fit
wave = wavelength
npeaks = N_ELEMENTS(peaks)
delta = PTRARR(npeaks)
mes2Theta = PTRARR(npeaks)
ndeltapeak = intarr(npeaks)
ndelta = 0
for i=0, npeaks-1 do begin
	index = self->findPeak(peaks[i])
	delta[i] = ptr_new((*self.subpatterns)(index[0])->getDelta(index[1]))
	mes2Theta[i] = ptr_new((*self.subpatterns)(index[0])->getTheta(index[1]))
	ndeltapeak[i] = N_ELEMENTS(*delta[i])
	ndelta += ndeltapeak[i]
endfor
; For each peak, we fit d0 and Q + 1 parameter for offset
; Starting values are
;   - d0: av(d)
;   - Q: 0
;   - offset: offsetstart, converted to radians
guess = fltarr(2*npeaks+1)
guess[0] = !PI*offsetstart/180.
for i=1, npeaks do begin
	guess[2*(i-1)+1] = mean(wavelength/(2.*sin(!PI*(*mes2Theta[i-1])/360.))) ; d0 for peak i
	guess[2*(i-1)+2] = 0. ; Q for peak i
end
; creating x and y data to fit
x = fltarr(ndelta)
y = fltarr(ndelta)
step = 0
; In order to set the fit data, we simply put all 2theta vs. delta one peak after the other-1 do begin
for i=0, npeaks-1 do begin
	for j=0, ndeltapeak[i]-1 do begin
		x[step] = !PI*(*delta[i])[j]/180.
		y[step] = !PI*(*mes2Theta[i])[j]/180.
		;
		step += 1
	endfor
endfor
sigma = 0.0005*(y)
fit = MPFITFUN('LATTICESTRAINMULTIPLEPEAKSOFFSET', x, y, sigma, guess, perror = perror, /quiet)
;for i=1, npeaks do fit[2*i] = wavelength/(2.*sin(fit[2*i]))
return, {fit:fit , perror: perror}
end






; Returns azimtuh angles for peak number i
function FitPatternObject::getDelta, i
index = self->findPeak(i)
return, (*self.subpatterns)(index[0])->getDelta(index[1])
end

; Returns intensities of peak number i
function FitPatternObject::getIntensity, i
index = self->findPeak(i)
return, (*self.subpatterns)(index[0])->getIntensity(index[1])
end

; Returns 2theta of peak number i
function FitPatternObject::getTheta, i
index = self->findPeak(i)
return, (*self.subpatterns)(index[0])->getTheta(index[1])
end

; Returns intensities of peak number i at one angle
function FitPatternObject::getIntensityAtAngle, i, angle
index = self->findPeak(i)
angles = (*self.subpatterns)(index[0])->getDelta(index[1])
intensities = (*self.subpatterns)(index[0])->getIntensity(index[1])
j = WHERE(angles eq angle)
if j eq (-1) then return, !VALUES.F_NAN
return, intensities[j]
end

; Returns d-spacings for peak number i
; Parameter: wavelength
function FitPatternObject::getDSpacing, i, wavelength
dspacings = wavelength/(2.*sin(!PI*self->getTheta(i)/360.))
return, dspacings
end

; Returns d-spacings for peak number i, with a correct for center of the image plate
; Parameter: wavelength
function FitPatternObject::getDSpacingCorrectCenter, i, wavelength, centerx, centery
r0 = tan(!PI*self->getTheta(i)/180.)
delta = !PI*self->getDelta(i)/180.
r1 = r0 + centerx * cos(delta)  + centery * sin(delta)
theta = atan(r1)/2.
dspacings = wavelength/(2.*sin(theta))
return, dspacings
end

; Returns half-width of peak number i
function FitPatternObject::getHalfWidth, i
index = self->findPeak(i)
return, (*self.subpatterns)(index[0])->getHalfWidth(index[1])
end

; returns the number of peaks
function FitPatternObject::getNPeaks
npeaks = 0
for j=0, self.nsubpat-1 do begin
	npeaks += (*self.subpatterns)(j)->nPeaks()
endfor
return, npeaks
end

; Locates the supattern and peak number for a peak
function FitPatternObject::findPeak, i
npeaks = fltarr(self.nsubpat)
for j=0, self.nsubpat-1 do begin
	npeaks[j] = (*self.subpatterns)(j)->nPeaks()
endfor
total = 0
subpat = -1
while (total lt i+1) do begin
	subpat = subpat + 1
	total = total + npeaks[subpat]
endwhile
index = fix(i-total+npeaks[subpat])
return, [subpat,index]
end

; Loads results of a fit from an Ascii file
function FitPatternObject::readFromAscii, lun
	on_ioerror, bad
	; file version
	row = readascii(lun, com='#')
	if (fix(row) ne 1) then return, 'Sorry, we can only read file format 1 at this time'
	; peak profile
	row = readascii(lun,  com='#')
	self.peakprofile = fix(row)
	; number of subpatterns
	row = readascii(lun,  com='#')
	self.nsubpat = fix(row)
	self.subpatterns = PTR_NEW(OBJARR(self.nsubpat))
	; subpatterns
	for i=0, self.nsubpat-1 do begin
		;print, 'Reading subpattern' + STRING(i, /PRINT)
		(*(self.subpatterns))(i) = OBJ_NEW('FitSubPatternObject')
		noerror = (*self.subpatterns)(i)->readFromAscii(lun)
		if (noerror ne 1) then return, noerror
	endfor
RETURN, 1
bad: return, !ERR_STRING
end