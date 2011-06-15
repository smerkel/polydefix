function latticestrainMultiplePeaksCenter, x, A
common fitinfo, npeaks, ndeltapeak, wave, offset
; position of center
x0 = A[0]
y0 = A[1]
;
step = 0
mestwotheta = fltarr(N_ELEMENTS(x))
;print, A
;print, N_ELEMENTS(x), N_ELEMENTS(ximageplate)
for i=0, npeaks-1 do begin
	d0 = A[2*(i+1)]
	Q = A[2*(i+1)+1]
	;print, i, t0, Q
	for j=0, ndeltapeak[i]-1 do begin
		; in system properly centered
		cosdelta = cos(x[step]-offset)
		sindelta = sin(x[step]-offset)
		cosdelta0 = cos(x[step])
		sindelta0 = sin(x[step])
		theta = asin(wave/(2.*d0*(1.+Q*(1.-3.*cosdelta*cosdelta))))
		for k=0, 10 do theta = asin(wave/(2.*d0*(1.+Q*(1.-3.*(cosdelta*cos(theta))^2))))
		; in system that is off-center
		;xpixel = x0+2.*th*cos(x[offset])
		;ypixel = y0+2.*th*sin(x[offset])
		mestwotheta[step] = atan(tan(2.*theta)-x0*cosdelta0-y0*sindelta0)
		step += 1
	endfor
endfor
return, mestwotheta
end
