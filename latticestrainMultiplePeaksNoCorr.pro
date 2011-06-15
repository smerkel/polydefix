function latticestrainMultiplePeaksNoCorr, x, A
common fitinfo, npeaks, ndeltapeak, wave, offset
;
step = 0
twotheta = fltarr(N_ELEMENTS(x))
;print, A
;print, N_ELEMENTS(x), N_ELEMENTS(ximageplate)
for i=0, npeaks-1 do begin
	d0 = A[2*i]
	Q = A[2*i+1]
	;print, i, t0, Q
	for j=0, ndeltapeak[i]-1 do begin
		; in system properly centered
		cosdelta = cos(x[step]-offset)
		twotheta[step] = 2.*asin(wave/(2.*d0*(1.+Q*(1.-3.*cosdelta*cosdelta))))
		for k=0, 10 do twotheta[step] = 2.*asin(wave/(2.*d0*(1.+Q*(1.-3.*(cosdelta*cos(twotheta[step]/2.))^2))))
		step += 1
	endfor
endfor
return, twotheta
end
