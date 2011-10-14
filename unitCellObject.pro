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

PRO unitCellObject__DEFINE 
	struct = { unitCellObject, set: 0, name:'', symmetry:'', cell:PTR_NEW(), details:'', p:0.0, dp:0.0 }  
END

function unitCellObject::Init, symmetry, name
self.symmetry = symmetry
self.name = name
case self.symmetry of
	'cubic': self.cell = PTR_NEW(fltarr(1,2))
	'hexa':  self.cell = PTR_NEW(fltarr(2,2))
	'ortho':  self.cell = PTR_NEW(fltarr(3,2))
  'mono':  self.cell = PTR_NEW(fltarr(4,2))
	else: self.cell = PTR_NEW(fltarr(1,1))
endcase
return, 1
end

pro unitCellObject::cleanup
PTR_FREE, self.cell
end

pro unitCellObject::setFit, fit
if (self.symmetry eq 'hexa') then begin
	(*self.cell)[0,0] = fit[0]
	(*self.cell)[0,1] = fit[1]
	(*self.cell)[1,0] = fit[2]
	(*self.cell)[1,1] = fit[3]
endif else if (self.symmetry eq 'ortho') then begin
  (*self.cell)[0,0] = fit[0]
  (*self.cell)[0,1] = fit[1]
  (*self.cell)[1,0] = fit[2]
  (*self.cell)[1,1] = fit[3]
  (*self.cell)[2,0] = fit[4]
  (*self.cell)[2,1] = fit[5]
endif else if (self.symmetry eq 'mono') then begin
  (*self.cell)[0,0] = fit[0]
  (*self.cell)[0,1] = fit[1]
  (*self.cell)[1,0] = fit[2]
  (*self.cell)[1,1] = fit[3]
  (*self.cell)[2,0] = fit[4]
  (*self.cell)[2,1] = fit[5]
  (*self.cell)[3,0] = fit[6]
  (*self.cell)[3,1] = fit[7]
endif else if (self.symmetry eq 'cubic') then begin
	(*self.cell)[0,0] = fit[0]
	(*self.cell)[0,1] = fit[1]
endif
end

pro unitCellObject::setDetails, txt
self.details = txt
end

pro unitCellObject::setPressure, p
self.p = p[0]
self.dp = p[1]
end

function unitCellObject::getVolume
if (self.symmetry eq 'hexa') then begin
	v = (*self.cell)[0,0]*(*self.cell)[0,0]*(*self.cell)[1,0]*sin(!pi/3.)
	dv = sqrt( (2.*(*self.cell)[0,0]* (*self.cell)[0,1]*(*self.cell)[1,0]*sin(!pi/3.))^2 + $
		((*self.cell)[0,0]*(*self.cell)[0,0]*(*self.cell)[1,1]*sin(!pi/3.))^2 )
	return, [v,dv]
endif else if (self.symmetry eq 'ortho') then begin
  v = (*self.cell)[0,0]*(*self.cell)[1,0]*(*self.cell)[2,0]
  dv = sqrt( ((*self.cell)[0,0]*(*self.cell)[1,0]*(*self.cell)[2,1])^2 + $
    ((*self.cell)[1,0]*(*self.cell)[2,0]*(*self.cell)[0,1])^2 + $
    ((*self.cell)[0,0]*(*self.cell)[2,0]*(*self.cell)[1,1])^2 )
  return, [v,dv]
endif else if (self.symmetry eq 'mono') then begin
  v = (*self.cell)[0,0]*(*self.cell)[1,0]*(*self.cell)[2,0]
  dv = sqrt( ((*self.cell)[0,0]*(*self.cell)[1,0]*sin((*self.cell)[3,0])*(*self.cell)[2,1])^2 + $
    ((*self.cell)[1,0]*(*self.cell)[2,0]*sin((*self.cell)[3,0])*(*self.cell)[0,1])^2 + $
    ((*self.cell)[0,0]*(*self.cell)[2,0]*sin((*self.cell)[3,0])*(*self.cell)[1,1])^2 + $
    ((*self.cell)[0,0]*(*self.cell)[1,0]*(*self.cell)[2,0]*cos((*self.cell)[3,0])*(*self.cell)[3,1])^2  )
  return, [v,dv]
endif else if (self.symmetry eq 'cubic') then begin
	v = (*self.cell)[0,0]*(*self.cell)[0,0]*(*self.cell)[0,0]
	dv = 3.*(*self.cell)[0,0]*(*self.cell)[0,0]*(*self.cell)[0,1]
	return, [v,dv]
endif
return, [0.,0.]
end

function unitCellObject::getVolumeNoError
if (self.symmetry eq 'hexa') then begin
	v = (*self.cell)[0,0]*(*self.cell)[0,0]*(*self.cell)[1,0]*sin(!pi/3.)
	return, v
endif else if (self.symmetry eq 'ortho') then begin
  v = (*self.cell)[0,0]*(*self.cell)[1,0]*(*self.cell)[2,0]
  return, v
endif  else if (self.symmetry eq 'mono') then begin
  v = (*self.cell)[0,0]*(*self.cell)[1,0]*(*self.cell)[2,0]*sin((*self.cell)[3,0])
  return, v
endif else  if (self.symmetry eq 'cubic') then begin
	v = (*self.cell)[0,0]*(*self.cell)[0,0]*(*self.cell)[0,0]
	return, v
endif
return, 0.
end


; returns the value of the unit cell parameters number i
; e.g. i=0, a for cubic, i=0, a and i=1, c for hexagonal...
function unitCellObject::getCellParValue, i
case self.symmetry of
	'cubic': begin
		case i of
			0: return, (*self.cell)[0,0]
			else: return, 0
		endcase
		end
	'hexa':  begin
		case i of
			0: return, (*self.cell)[0,0]
			1: return, (*self.cell)[1,0]
			2: return, ((*self.cell)[1,0]/(*self.cell)[0,0])
			else: return, 0.
		endcase
		end
	'ortho':  begin
		case i of
			0: return, (*self.cell)[0,0]
			1: return, (*self.cell)[1,0]
			2: return, (*self.cell)[2,0]
			else: return, 0
		endcase
		end
  'mono':  begin
    case i of
      0: return, (*self.cell)[0,0]
      1: return, (*self.cell)[1,0]
      2: return, (*self.cell)[2,0]
      3: return, (*self.cell)[3,0]
      else: return, 0
    endcase
    end
	else: return, 0
endcase
return, 0
end

; returns the value of the error on unit cell parameters number i
; e.g. i=0, a for cubic, i=0, a and i=1, c for hexagonal...
function unitCellObject::getCellErrParValue, i
case self.symmetry of
	'cubic': begin
		case i of
			0: return, (*self.cell)[0,1]
			else: return, 0
		endcase
		end
	'hexa':  begin
		case i of
			0: return, (*self.cell)[0,1]
			1: return, (*self.cell)[1,1]
			2: return, sqrt( ((*self.cell)[1,1]/(*self.cell)[0,0])^2 +  ((*self.cell)[0,1]*(*self.cell)[1,0]/((*self.cell)[0,0]*(*self.cell)[0,0]))^2)
			else: return, 0.
		endcase
		end
	'ortho':  begin
		case i of
			0: return, (*self.cell)[0,1]
			1: return, (*self.cell)[1,1]
			2: return, (*self.cell)[2,1]
			else: return, 0
		endcase
		end
  'mono':  begin
    case i of
      0: return, (*self.cell)[0,1]
      1: return, (*self.cell)[1,1]
      2: return, (*self.cell)[2,1]
      3: return, (*self.cell)[3,1]
      else: return, 0
    endcase
    end
	else: return, 0
endcase
return, 0
end

function unitCellObject::getDHKL, h, k, l
if (self.symmetry eq 'hexa') then begin
	tmp = 4.*(h*h+h*k+k*k)/(3.*(*self.cell)[0,0]*(*self.cell)[0,0])+l*l/((*self.cell)[1,0]*(*self.cell)[1,0])
	return, 1./sqrt(tmp)
endif else if (self.symmetry eq 'mono') then begin
  tmp = h*h/((*self.cell)[0,0]*sin((*self.cell)[3,0]))^2 + k*k/((*self.cell)[1,0])^2 + l*l/((*self.cell)[2,0]*sin((*self.cell)[3,0]))^2 - 2.*h*l*cos((*self.cell)[3,0])/((*self.cell)[0,0]*(*self.cell)[2,0]*(sin((*self.cell)[3,0]))^2)
  return, 1./sqrt(tmp)
endif else if (self.symmetry eq 'ortho') then begin
  tmp = h*h/((*self.cell)[0,0])^2 + k*k/((*self.cell)[1,0])^2 + l*l/((*self.cell)[2,0])^2 
  return, 1./sqrt(tmp)
endif  else if (self.symmetry eq 'cubic') then begin
	tmp = (h*h+k*k+l*l)/((*self.cell)[0,0]*(*self.cell)[0,0])
	return, 1./sqrt(tmp)
endif
return, 0.
end

function unitCellObject::getPressure
return, self.P
end

function unitCellObject::getErrPressure
return, self.dP
end

function unitCellObject::getName
return, self.name
end

function unitCellObject::summaryLong
txt = self.name + "\n"
if (self.symmetry eq 'hexa') then $
	txt += "a = " + fltformatA((*self.cell)[0,0]) + " (+/-) "+ fltformatA((*self.cell)[0,1]) + "\n" +$
		"c = " + fltformatA((*self.cell)[1,0]) + " (+/-) "+ fltformatA((*self.cell)[1,1]) + "\n" + $
		"c/a = " +  fltformatA((*self.cell)[1,0]/(*self.cell)[0,0]) + "\n"
if (self.symmetry eq 'ortho') then $
  txt += "a = " + fltformatA((*self.cell)[0,0]) + " (+/-) " + fltformatA((*self.cell)[0,1]) + "\n" +$
    "b = " + fltformatA((*self.cell)[1,0]) + " (+/-) " + fltformatA((*self.cell)[1,1]) + "\n" + $
    "c = " +  fltformatA((*self.cell)[2,0]) + " (+/-) " + fltformatA((*self.cell)[2,1]) +  "\n"
if (self.symmetry eq 'mono') then $
  txt += "a = " + fltformatA((*self.cell)[0,0]) + " (+/-) " + fltformatA((*self.cell)[0,1]) + "\n" +$
    "b = " + fltformatA((*self.cell)[1,0]) + " (+/-) " + fltformatA((*self.cell)[1,1]) + "\n" + $
    "c = " +  fltformatA((*self.cell)[2,0]) + " (+/-) " + fltformatA((*self.cell)[2,1]) +  "\n" + $
    "beta = " +  fltformatA((*self.cell)[3,0]*180./!PI) + " (+/-) " + fltformatA((*self.cell)[3,1]*180./!PI) +  "\n"
if (self.symmetry eq 'cubic') then $
	txt += "a = " + fltformatA((*self.cell)[0,0]) + " (+/-) "+ fltformatA((*self.cell)[0,1]) + "\n"
txt += self.details
return, txt
end

function unitCellObject::summaryPressure
txt = self.name + "\n"
if (self.symmetry eq 'hexa') then $
  txt += "a = " + fltformatA((*self.cell)[0,0]) + " (+/-) "+ fltformatA((*self.cell)[0,1]) + "\n" +$
    "c = " + fltformatA((*self.cell)[1,0]) + " (+/-) "+ fltformatA((*self.cell)[1,1]) + "\n" + $
    "c/a = " +  fltformatA((*self.cell)[1,0]/(*self.cell)[0,0]) + "\n"
if (self.symmetry eq 'ortho') then $
  txt += "a = " + fltformatA((*self.cell)[0,0]) + " (+/-) " + fltformatA((*self.cell)[0,1]) + "\n" +$
    "b = " + fltformatA((*self.cell)[1,0]) + " (+/-) " + fltformatA((*self.cell)[1,1]) + "\n" + $
    "c = " +  fltformatA((*self.cell)[2,0]) + " (+/-) " + fltformatA((*self.cell)[2,1]) +  "\n"
if (self.symmetry eq 'mono') then $
  txt += "a = " + fltformatA((*self.cell)[0,0]) + " (+/-) " + fltformatA((*self.cell)[0,1]) + "\n" +$
    "b = " + fltformatA((*self.cell)[1,0]) + " (+/-) " + fltformatA((*self.cell)[1,1]) + "\n" + $
    "c = " +  fltformatA((*self.cell)[2,0]) + " (+/-) " + fltformatA((*self.cell)[2,1]) +  "\n" + $
    "beta = " +  fltformatA((*self.cell)[3,0]*180./!PI) + " (+/-) " + fltformatA((*self.cell)[3,1]*180./!PI) +  "\n"
if (self.symmetry eq 'cubic') then $
  txt += "a = " + fltformatA((*self.cell)[0,0]) + " (+/-) "+ fltformatA((*self.cell)[0,1]) + "\n"
txt += "P = " + fltformatA(self.P) + " (+/-) "+ fltformatA(self.dp) + "\n" 
return, txt
end


function unitCellObject::summaryPCSV
V = self->getVolume()
txt = ""
if (self.symmetry eq 'hexa') then $
	txt += fltformatA((*self.cell)[0,0]) + STRING(9B) + fltformatA((*self.cell)[0,1]) + STRING(9B) + fltformatA((*self.cell)[1,0]) +  STRING(9B)+ fltformatA((*self.cell)[1,1]) + STRING(9B) + fltformatA(V[0]) + STRING(9B) + fltformatA(V[1]) + STRING(9B) +  fltformatA(self.P) + STRING(9B) + fltformatA(self.dp)
if (self.symmetry eq 'ortho') then $
  txt += fltformatA((*self.cell)[0,0]) + STRING(9B) + fltformatA((*self.cell)[0,1]) + STRING(9B) + fltformatA((*self.cell)[1,0]) +  STRING(9B)+ fltformatA((*self.cell)[1,1]) + STRING(9B)+ fltformatA((*self.cell)[2,0]) +  STRING(9B)+ fltformatA((*self.cell)[2,1]) + STRING(9B) + fltformatA(V[0]) + STRING(9B) + fltformatA(V[1]) + STRING(9B) +  fltformatA(self.P) + STRING(9B) + fltformatA(self.dp)
if (self.symmetry eq 'mono') then $
   txt += fltformatA((*self.cell)[0,0]) + STRING(9B) + fltformatA((*self.cell)[0,1]) + STRING(9B) + fltformatA((*self.cell)[1,0]) +  STRING(9B)+ fltformatA((*self.cell)[1,1]) + STRING(9B)+ fltformatA((*self.cell)[2,0]) +  STRING(9B)+ fltformatA((*self.cell)[2,1]) +  STRING(9B)+ fltformatA((*self.cell)[3,0]*180./!PI) +  STRING(9B)+ fltformatA((*self.cell)[3,1]*180./!PI) + $ 
    STRING(9B) + fltformatA(V[0]) + STRING(9B) + fltformatA(V[1]) + STRING(9B) +  fltformatA(self.P) + STRING(9B) + fltformatA(self.dp) 
if (self.symmetry eq 'cubic') then $
	txt += fltformatA((*self.cell)[0,0]) + STRING(9B) + fltformatA((*self.cell)[0,1]) + STRING(9B) + fltformatA(V[0]) + STRING(9B) + fltformatA(V[1]) + STRING(9B) +  fltformatA(self.P) + STRING(9B) + fltformatA(self.dp)
return, txt
end
