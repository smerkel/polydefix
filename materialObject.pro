
; symmetry codes (numbers and letters, symmetry code in letters in the reference)
; - 0 cubic
; - 1 hexa for hexagonal
; - 2 otho for orthorhombic
; Equation of state (Birch-Mur...)
; - V0: unit cell volume at zero pressure
; - K0: bulk modulus at zero pressure
; - dK0: first derivative of bulk modulus 
; elasticmodel: 
;  0->isotropic
;  1->anisotropic
; Parameters for isotropic elastic model
;  iK[]: K = iK[0] + iK[1] * P + iK[2] * P*P  (P deduced from the data and EOS)
;  iG[]: G = iG[0] + iG[1] * P + iG[2] * P*P  (P deduced from the data and EOS)
; Parameters for anisotropic elastic model
;  Cij: Cij = Cij[i,j,0] + Cij[i,j,1] * P + Cij[i,j,2] * P * P(P deduced from the data and EOS)

PRO materialObject__DEFINE 
	struct = { materialObject, tmp: 0, name:'Not set', symmetry:'', V0:0.0, K0:0.0, dK0:0.0, elasticmodel: 0, iK: fltarr(3), iG: fltarr(3), Cij: fltarr(7,7,3) }
END

function materialObject::Init
for i=0,2 do begin
	self.iK[i] = 0.0
	self.iG[i] = 0.0
endfor
for i=0,6 do begin
	for j=0,6 do begin
		for k=0,2 do begin
			self.Cij[i,j,k] = 0.0
		endfor
	endfor
endfor
return, 1
end

; *************************************************** Temporary variables **********************

; functions with the tmp variable: usefull to transmit information from actions
; with the object itself...
pro materialObject::setTmp, tmp
self.tmp = tmp
end

function materialObject::getTmp
return, self.tmp
end

; **************************************************** Elastic properties ***********************

pro materialObject::setIsotropicProp, g0, g1, g2
self.iK[0] = 0.
self.iK[1] = 0.
self.iK[2] = 0.
self.iG[0] = g0
self.iG[1] = g1
self.iG[2] = g2
self.elasticmodel=0
end

pro materialObject::setAnisPropHexa, c11,  c33,  c12,  c13,  c44,  dc11,  dc33,  dc12,  dc13,  dc44,   ddc11,  ddc33,  ddc12,  ddc13,  ddc44
for i=0,6 do begin
	for j=0,6 do begin
		for k=0,2 do begin
			self.Cij[i,j,k] = 0.0
		endfor
	endfor
endfor
self.Cij[1,1,0] = c11
self.Cij[2,2,0] = c11
self.Cij[3,3,0] = c33
self.Cij[1,2,0] = c12
self.Cij[1,3,0] = c13
self.Cij[2,3,0] = c13
self.Cij[4,4,0] = c44
self.Cij[5,5,0] = c44
self.Cij[6,6,0] = 0.5*(c11-c12)
self.Cij[1,1,1] = dc11
self.Cij[2,2,1] = dc11
self.Cij[3,3,1] = dc33
self.Cij[1,2,1] = dc12
self.Cij[1,3,1] = dc13
self.Cij[2,3,1] = dc13
self.Cij[4,4,1] = dc44
self.Cij[5,5,1] = dc44
self.Cij[6,6,1] = 0.5*(dc11-dc12)
self.Cij[1,1,2] = ddc11
self.Cij[2,2,2] = ddc11
self.Cij[3,3,2] = ddc33
self.Cij[1,2,2] = ddc12
self.Cij[1,3,2] = ddc13
self.Cij[2,3,2] = ddc13
self.Cij[4,4,2] = ddc44
self.Cij[5,5,2] = ddc44
self.Cij[6,6,2] = 0.5*(ddc11-ddc12)
for i=1,6 do begin
	for j=1,i-1 do begin
		for k=0,2 do begin
			self.Cij[i,j,k] = self.Cij[j,i,k] 
		endfor
	endfor
endfor
self.elasticmodel=1
end


pro materialObject::setAnisPropCubic, c11,  c12, c44,  dc11,  dc12,  dc44,   ddc11, ddc12,  ddc44
for i=0,6 do begin
	for j=0,6 do begin
		for k=0,2 do begin
			self.Cij[i,j,k] = 0.0
		endfor
	endfor
endfor
self.Cij[1,1,0] = c11
self.Cij[2,2,0] = c11
self.Cij[3,3,0] = c11
self.Cij[1,2,0] = c12
self.Cij[1,3,0] = c12
self.Cij[2,3,0] = c12
self.Cij[4,4,0] = c44
self.Cij[5,5,0] = c44
self.Cij[6,6,0] = c44
self.Cij[1,1,1] = dc11
self.Cij[2,2,1] = dc11
self.Cij[3,3,1] = dc11
self.Cij[1,2,1] = dc12
self.Cij[1,3,1] = dc12
self.Cij[2,3,1] = dc12
self.Cij[4,4,1] = dc44
self.Cij[5,5,1] = dc44
self.Cij[6,6,1] = dc44
self.Cij[1,1,2] = ddc11
self.Cij[2,2,2] = ddc11
self.Cij[3,3,2] = ddc11
self.Cij[1,2,2] = ddc12
self.Cij[1,3,2] = ddc12
self.Cij[2,3,2] = ddc12
self.Cij[4,4,2] = ddc44
self.Cij[5,5,2] = ddc44
self.Cij[6,6,2] = ddc44
for i=1,6 do begin
	for j=1,i-1 do begin
		for k=0,2 do begin
			self.Cij[i,j,k] = self.Cij[j,i,k] 
		endfor
	endfor
endfor
self.elasticmodel=1
end

;  **************************************************** Set parameters **************************

pro materialObject::setSymmetryFromCode, code
case code  OF
	0: self.symmetry = 'cubic'
	1: self.symmetry = 'hexa'
	2: self.symmetry = 'ortho'
	else:
endcase
end

pro materialObject::setElasticModel, model
self.elasticmodel = model
end

pro materialObject::setName, name
self.name = name
end

pro materialObject::setEOSParameters, vo, ko, dko
self.V0 = vo
self.K0 = ko
self.dK0 = dKo
end

; **************************************************** Information ******************************

function materialObject::refineUnitCell, latticestrain
cell = OBJ_NEW('unitCellObject', self.symmetry, latticestrain->getName())
if (latticestrain->getSet() eq 1) then begin
	if(self.symmetry eq 'hexa') then begin 
		nuse = latticestrain->getnuse()
		d = latticestrain->getd()
		dd = latticestrain->getdd()
		h = latticestrain->geth()
		k = latticestrain->getk()
		l = latticestrain->getl()
		x = replicate({plane, h:0, k:0, l:0},nuse)
		for i=0,nuse-1 do begin
			x[i] = {plane, h[i], k[i], l[i]}
		endfor
		a = [1., 1.6]
		fit = MPFITFUN('dhklhexa', x, d, dd, a, perror=perror, YFIT=dfit, /quiet)
		cell->setFit, [fit[0], perror[0], fit[1], perror[1]]
		txt = "\thkl    dm       dc      diff\n"
		for i=0,nuse-1 do begin
			txt += "\t" + STRTRIM(STRING(h[i],/PRINT),2)+ STRTRIM(STRING(k[i],/PRINT),2)+ STRTRIM(STRING(l[i],/PRINT),2) + ": " + fltformatA(d[i]) + " (+/-) " + fltformatA(dd[i]) + "     " + fltformatA(dfit[i]) + "    " + fltformatA(d[i]-dfit[i]) + "\n" 
		endfor
		cell->setDetails, txt
	endif else if (self.symmetry eq 'cubic') then begin 
		nuse = latticestrain->getnuse()
		d = latticestrain->getd()
		dd = latticestrain->getdd()
		h = latticestrain->geth()
		k = latticestrain->getk()
		l = latticestrain->getl()
		x = replicate({plane, h:0, k:0, l:0},nuse)
		for i=0,nuse-1 do begin
			x[i] = {plane, h[i], k[i], l[i]}
		endfor
		a = d[0]*sqrt(h[0]*h[0]+k[0]*k[0]+l[0]*l[0])
		fit = MPFITFUN('dhklcubic', x, d, dd, a, perror=perror, YFIT=dfit, /quiet)
		cell->setFit, [fit[0], perror[0]]
		txt = "\thkl    dm       dc      diff\n"
		for i=0,nuse-1 do begin
			txt += "\t" + STRTRIM(STRING(h[i],/PRINT),2)+ STRTRIM(STRING(k[i],/PRINT),2)+ STRTRIM(STRING(l[i],/PRINT),2) + ": " + fltformatA(d[i]) + " (+/-) " + fltformatA(dd[i]) + "     " + fltformatA(dfit[i]) + "    " + fltformatA(d[i]-dfit[i]) + "\n" 
		endfor
		cell->setDetails, txt
	endif
endif
return, cell
end

function materialObject::birch3, volume
v = volume/self.V0
f = .5*(v^(-2./3.)-1.);
p0 = self.K0;
p1 = 1.5*self.K0*(self.dK0-4.);
FF = p0+p1*f;
p = FF*3.*f*(1.+2.*f)^2.5;
return, p
end

function materialObject::refinePressure, latticestrain
cell = self->refineUnitCell(latticestrain)
V = cell->getVolume()
P = self->birch3(V[0])
PP1 = self->birch3(V[0]+V[1])
PP2 = self->birch3(V[0]-V[1])
dP = 0.5*abs(PP1-PP2)
cell->setPressure, [P,dP]
return, cell
end

function materialObject::refineVolume, latticestrain
cell = self->refineUnitCell(latticestrain)
return, cell
end

; **************************************************** Information ******************************

function materialObject::getName
return, self.name
end

function materialObject::getSymmetry
return, self.symmetry
end

function materialObject::getSymmetryFromCode, code
case code  OF
	0: return, 'cubic'
	1: return, 'hexa'
	2: return, 'ortho'
	else: return, 10
endcase
return, 10
end

function materialObject::getSymmetryCode
case self.symmetry of
	'cubic': return, 0
	'hexa': return, 1
	'ortho': return, 2
	else: return, 10
endcase
return, 10
end

function materialObject::getElasticModelCode
return, self.elasticmodel
end

function materialObject::getV0
return, self.V0
end

function materialObject::getK0
return, self.K0
end

function materialObject::getDK0
return, self.dK0
end

function materialObject::getIK, i
return, self.iK[i]
end

function materialObject::getIG, i
return, self.iG[i]
end

function materialObject::getCij, i, j, k
return, self.Cij[i,j,k]
end


function materialObject::infoTxt
str = "Information about this material:\n"
str += "\tName: " + self.name + "\n"
str += "\tSymmetry: " + self.symmetry + "\n"
str += "\tEquation of state parameters: Vo=" +  STRTRIM(STRING(self.V0,/PRINT),2) +  " Ko=" +  STRTRIM(STRING(self.K0,/PRINT),2) +   " K'o=" +  STRTRIM(STRING(self.dK0,/PRINT),2) + "\n"
if (self.elasticmodel eq 0) then begin
	str += "\tElastic model: isotropic\n"
	str += "\tG = " + STRTRIM(STRING(self.iG[0],/PRINT),2) + " + " + STRTRIM(STRING(self.iG[1],/PRINT),2) + "*P + " + STRTRIM(STRING(self.iG[2],/PRINT),2) + "*P*P\n"
endif else begin
	str += "\tElastic model: anisotropic\n"
	if (self.symmetry eq 'hexa') then begin
		str += "\tC11 = " + STRTRIM(STRING(self.Cij[1,1,0],/PRINT),2) + " + " + STRTRIM(STRING(self.Cij[1,1,1],/PRINT),2) + "*P + " + STRTRIM(STRING(self.Cij[1,1,2],/PRINT),2) + "*P*P\n"
		str += "\tC33 = " + STRTRIM(STRING(self.Cij[3,3,0],/PRINT),2) + " + " + STRTRIM(STRING(self.Cij[3,3,1],/PRINT),2) + "*P + " + STRTRIM(STRING(self.Cij[3,3,2],/PRINT),2) + "*P*P\n"
		str += "\tC12 = " + STRTRIM(STRING(self.Cij[1,2,0],/PRINT),2) + " + " + STRTRIM(STRING(self.Cij[1,2,1],/PRINT),2) + "*P + " + STRTRIM(STRING(self.Cij[1,2,2],/PRINT),2) + "*P*P\n"
		str += "\tC13 = " + STRTRIM(STRING(self.Cij[1,3,0],/PRINT),2) + " + " + STRTRIM(STRING(self.Cij[1,3,1],/PRINT),2) + "*P + " + STRTRIM(STRING(self.Cij[1,3,2],/PRINT),2) + "*P*P\n"
		str += "\tC44 = " + STRTRIM(STRING(self.Cij[4,4,0],/PRINT),2) + " + " + STRTRIM(STRING(self.Cij[4,4,1],/PRINT),2) + "*P + " + STRTRIM(STRING(self.Cij[4,4,2],/PRINT),2) + "*P*P\n"
	endif else if (self.symmetry eq 'cubic') then begin
		str += "\tC11 = " + STRTRIM(STRING(self.Cij[1,1,0],/PRINT),2) + " + " + STRTRIM(STRING(self.Cij[1,1,1],/PRINT),2) + "*P + " + STRTRIM(STRING(self.Cij[1,1,2],/PRINT),2) + "*P*P\n"
		str += "\tC12 = " + STRTRIM(STRING(self.Cij[1,2,0],/PRINT),2) + " + " + STRTRIM(STRING(self.Cij[1,2,1],/PRINT),2) + "*P + " + STRTRIM(STRING(self.Cij[1,2,2],/PRINT),2) + "*P*P\n"
		str += "\tC44 = " + STRTRIM(STRING(self.Cij[4,4,0],/PRINT),2) + " + " + STRTRIM(STRING(self.Cij[4,4,1],/PRINT),2) + "*P + " + STRTRIM(STRING(self.Cij[4,4,2],/PRINT),2) + "*P*P\n"
	endif 
endelse
return, str
end

function materialObject::labelPCSV
case self.symmetry of
	'cubic': return, "#" + STRING(9B) + "a"  + STRING(9B) + "da" + STRING(9B)+ "V" + STRING(9B) + "dV"  + STRING(9B) + "P" + STRING(9B) + "dP"
	'hexa': return, "#" + STRING(9B) + "a" + STRING(9B) + "da" + STRING(9B) + "c" + STRING(9B) + "dc"  + STRING(9B) + "V" + STRING(9B) + "dV" + STRING(9B) + "P" + STRING(9B) + "dP"
	'ortho': return, "#" + STRING(9B) + "a" + STRING(9B) + "da" + STRING(9B) + "b" + STRING(9B) + "db" + STRING(9B) + "c" + STRING(9B) + "dc" + STRING(9B) + "V" + STRING(9B) + "dV" + STRING(9B) + "P" + STRING(9B) + "dP"
	else: return, 10
endcase
return, 10
end

; ***************************************************  cell parameter stuff ********

; returns an array of string with the name of the unit cell parameters
; e.g. a for cubic, a and c for hexagonal...
function materialObject::getCellParList
case self.symmetry of
	'cubic': return, ['a']
	'hexa': return, ['a', 'c', 'c/a']
	'ortho': return, ['a', 'b' ,'c']
	else: return, ['']
endcase
return, ['']
end

; returns the name of the unit cell parameters number i
; e.g. i=0, a for cubic, i=0, a and i=1, c for hexagonal...
function materialObject::getCellParName, i
case self.symmetry of
	'cubic': begin
		case i of
			0: return, 'a'
			else: return, ''
		endcase
		end
	'hexa':  begin
		case i of
			0: return, 'a'
			1: return, 'c'
			2: return, 'c/a'
			else: return, ''
		endcase
		end
	'ortho':  begin
		case i of
			0: return, 'a'
			1: return, 'b'
			2: return, 'c'
			else: return, ''
		endcase
		end
	else: return, ''
endcase
return, ''
end


; ***************************************************  stresses ******************

function materialObject::twoGReussCubic, h, k, l, a, p
Cmatrix = fltarr(6,6)
for i=1,6 do begin
	for j=1,6 do begin
		Cmatrix[i-1,j-1] = self.Cij[i,j,0] + p * self.Cij[i,j,1] + self.Cij[i,j,2] * p * p
	endfor
endfor
; print, 'hkl a  p', h, k, l, a, p
; print, 'Cij', self.Cij[*,*,0]
; print, 'C', Cmatrix
S = INVERT(Cmatrix)
tmp1 = h*h + k*k + l*l
tmp2 = h*h*k*k + k*k*l*l + l*l*h*h
Gamma = 1.*tmp2/(tmp1*tmp1)
; print, 'Gamma', Gamma
inv = (S[0,0]-S[0,1]) - 3.*(S[0,0]-S[0,1]-0.5*S[3,3])*Gamma
; print, '2G' , 1./inv
return, 1./inv
end




function materialObject::twoGReussHexa, h, k, l, a, c, p
Cmatrix = fltarr(6,6)
for i=1,6 do begin
	for j=1,6 do begin
		Cmatrix[i-1,j-1] = self.Cij[i,j,0] + p * self.Cij[i,j,1] + self.Cij[i,j,2]
	endfor
endfor
; print, 'hkl a c p', h, k, l, a, c, p
; print, 'Cij', self.Cij[*,*,0]
; print, 'C', Cmatrix
S = INVERT(Cmatrix)
M = 4.*c*c*(h*h+h*k+k*k)+3.*a*a*l*l
ll = 3.*a*a*l*l/M
inv = 0.5 * (2.*S[0,0] - S[0,1] - S[0,2]) $
		+ ll* (-5.*S[0,0] + S[0,1] + 5.*S[0,2] - S[2,2] + 3.*S[3,3]) $
		+ ll*ll* (3.*S[0,0] - 6.*S[0,2] + 3.*S[2,2] - 3.*S[3,3])
; print, '2G' , 1./inv
return, 1./inv
end

function materialObject::errTwoGReussCubic, h, k, l, a, p, da, dp
d1 = (self->twoGReussCubic(h, k, l, 1.01*a, p) - self->twoGReussCubic(h, k, l, 0.99*a,p)) / (0.02 * a)
d3 = (self->twoGReussCubic(h, k, l, a,1.01*p) - self->twoGReussCubic(h, k, l, a, 0.99*p)) / (0.02 * p)
err = sqrt( (d1*da)^2 + (d3*dp)^2 )
return, err
end


function materialObject::errTwoGReussHexa, h, k, l, a, c, p, da, dc, dp
d1 = (self->twoGReussHexa(h, k, l, 1.01*a, c, p) - self->twoGReussHexa(h, k, l, 0.99*a, c, p)) $
			/ (0.02 * a)
d2 = (self->twoGReussHexa(h, k, l, a, 1.01*c, p) - self->twoGReussHexa(h, k, l, a, 0.99*c, p)) $
			/ (0.02 * c)
d3 = (self->twoGReussHexa(h, k, l, a, c, 1.01*p) - self->twoGReussHexa(h, k, l, a, c, 0.99*p)) $
			/ (0.02 * p)
err = sqrt( (d1*da)^2 + (d2*dc)^2 + (d3*dp)^2 )
return, err
end


; returns an array of string with the name of the unit cell parameters
; e.g. a for cubic, a and c for hexagonal...
function materialObject::getTwoG, h, k, l, cell
if ((self.elasticmodel eq 0) or (self.symmetry eq 'ortho')) then begin
  p = cell->getPressure()
  twoG = 2.*(self.iG[0] + p*self.iG[1] + p*p*self.iG[2])
  return, twoG
endif else begin
  case self.symmetry of
	'cubic': begin
		a = cell->getCellParValue(0)
		p = cell->getPressure()
		return, self->twoGReussCubic(h, k, l, a, p)
	end
	'hexa': begin
		a = cell->getCellParValue(0)
		c = cell->getCellParValue(1)
		p = cell->getPressure()
		; print, 'hkl a c p', h, k, l, a, c, p
		return, self->twoGReussHexa(h, k, l, a, c, p)
	end
	else: return, 0.
  endcase
endelse
return, 0.
end

; returns an array of string with the name of the unit cell parameters
; e.g. a for cubic, a and c for hexagonal...
function materialObject::getErrTwoG, h, k, l, cell
if ((self.elasticmodel eq 0) or (self.symmetry eq 'ortho')) then return, 0.
case self.symmetry of
	'cubic': begin
		a = cell->getCellParValue(0)
		da = cell->getCellErrParValue(0)
		p = cell->getPressure()
		dp = cell->getErrPressure()
		return, self->errTwoGReussCubic(h, k, l, a, p, da, dp)
	end
	'hexa': begin
		a = cell->getCellParValue(0)
		c = cell->getCellParValue(1)
		da = cell->getCellErrParValue(0)
		dc = cell->getCellErrParValue(1)
		p = cell->getPressure()
		dp = cell->getErrPressure()
		return, self->errTwoGReussHexa(h, k, l, a, c, p, da, dc, dp)
	end
	'ortho': return, 0
	else: return, 0
endcase
return, 0
end

; *************************************************** ASCII Import and Export ****************

function materialObject::saveToAscii, lun
printf, lun, '# Material properties'
printf, lun, '# Name'
printf, lun, self.name
printf, lun, '# Symmetry'
printf, lun, self.symmetry
printf, lun, "# EOS stuff (v0, k0, k'0)"
printf, lun, STRING(self.V0, /PRINT) + STRING(self.K0, /PRINT) + STRING(self.dK0, /PRINT)
printf, lun, '# Elastic model'
printf, lun, STRING(self.elasticmodel, /PRINT)
printf, lun, '# Parameters for isotropic elastic model (k0 k1 k2, g0, g1, g2)'
printf, lun, STRING(self.iK[0], /PRINT) + STRING(self.iK[1], /PRINT) +  STRING(self.iK[2], /PRINT) 
printf, lun, STRING(self.iG[0], /PRINT) + STRING(self.iG[1], /PRINT) +  STRING(self.iG[2], /PRINT)
printf, lun, '# Parameters for anisotropic elastic model (Cij)'
for i=1,6 do printf, lun, STRING(self.Cij[i,1,0], /PRINT) + STRING(self.Cij[i,2,0], /PRINT) +  STRING(self.Cij[i,3,0], /PRINT) + STRING(self.Cij[i,4,0], /PRINT) + STRING(self.Cij[i,5,0], /PRINT) +  STRING(self.Cij[i,6,0], /PRINT) 
printf, lun, '# Parameters for anisotropic elastic model (dCij/dp)'
for i=1,6 do printf, lun, STRING(self.Cij[i,1,1], /PRINT) + STRING(self.Cij[i,2,1], /PRINT) +  STRING(self.Cij[i,3,1], /PRINT) + STRING(self.Cij[i,4,1], /PRINT) + STRING(self.Cij[i,5,1], /PRINT) +  STRING(self.Cij[i,6,1], /PRINT) 
printf, lun, '# Parameters for anisotropic elastic model (d2Cij/dp2)'
for i=1,6 do printf, lun, STRING(self.Cij[i,1,2], /PRINT) + STRING(self.Cij[i,2,2], /PRINT) +  STRING(self.Cij[i,3,2], /PRINT) + STRING(self.Cij[i,4,2], /PRINT) + STRING(self.Cij[i,5,2], /PRINT) +  STRING(self.Cij[i,6,2], /PRINT)
RETURN, 1
end

function materialObject::readFromAscii, lun
	on_ioerror, bad
	; name
	self.name = STRTRIM(readascii(lun,com='#'),2)
	; symmetry
	self.symmetry = STRTRIM(readascii(lun,com='#'),2)
	; EOS stuff
	row = strsplit(readascii(lun, com='#'), /extract)
	self.V0 = float(row[0])
	self.K0 = float(row[1])
	self.dK0 = float(row[2])
	; elastic model
	self.elasticmodel = fix(readascii(lun, com='#'))
	; parameters for isotropic elastic model
	row = strsplit(readascii(lun, com='#'), /extract)
	for i=0,2 do self.iK[i] = row[i]
	row = strsplit(readascii(lun, com='#'), /extract)
	for i=0,2 do self.iG[i] = row[i]
	; parameters for anisotropic elastic model
	for k = 0, 2 do begin
		for i = 1,6 do begin
			row = strsplit(readascii(lun, com='#'), /extract)
			for j=1,6 do begin
				self.Cij[i,j,k] = row[j-1]
			endfor
		endfor
	endfor
RETURN, 1
bad: return, !ERR_STRING
end