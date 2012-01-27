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


; ********************************************** Isotropic elastic properties dialog **********

PRO doChgElPropIso, input, g0St, g1St, g2St
common materialwindow, material
WIDGET_CONTROL, g0St, GET_VALUE=g0S
WIDGET_CONTROL, g1St, GET_VALUE=g1S
WIDGET_CONTROL, g2St, GET_VALUE=g2S
g0 = float(g0S)
g1 = float(g1S)
g2 = float(g2S)
material->setIsotropicProp, g0, g1, g2
WIDGET_CONTROL, input, /DESTROY
END

PRO chgElPropIsoWindow_event, ev
; Get the 'stash' structure.
WIDGET_CONTROL, ev.TOP, GET_UVALUE=stash
WIDGET_CONTROL, ev.ID, GET_UVALUE=uval
CASE ev.id OF
	stash.input:
	else: begin
		CASE uval OF
		'OK': doChgElPropIso, stash.input, stash.g0St, stash.g1St, stash.g2St
		'CANCEL': WIDGET_CONTROL, stash.input, /DESTROY
		else:
		ENDCASE
	endcase
endcase
END

PRO chgElPropIsoWindow, base
common materialwindow, material
common fonts, titlefont, boldfont, mainfont
; base GUI
input = WIDGET_BASE(Title='Isotropic elastic properties', /COLUMN, /MODAL, GROUP_LEADER=base)
inputLa = WIDGET_LABEL(input, VALUE='Isotropic elastic properties', /ALIGN_CENTER, font=titlefont)
; Material properties
mat = WIDGET_BASE(input, /COLUMN, FRAME=1)
; Main properties
main = WIDGET_BASE(mat, COLUMN=4, /GRID_LAYOUT, FRAME=0)
dummyLa = WIDGET_LABEL(main, VALUE='', /ALIGN_LEFT)
; kLa = WIDGET_LABEL(main, VALUE='K', /ALIGN_LEFT)
gLa = WIDGET_LABEL(main, VALUE='G', /ALIGN_LEFT)
dummyLa = WIDGET_LABEL(main, VALUE='Coef0', /ALIGN_LEFT)
; k0St = WIDGET_TEXT(main, VALUE=STRTRIM(STRING(material->getIK(0),/PRINT),2), XSIZE=10, /EDITABLE)
g0St = WIDGET_TEXT(main, VALUE=STRTRIM(STRING(material->getIG(0),/PRINT),2), XSIZE=10, /EDITABLE)
dummyLa = WIDGET_LABEL(main, VALUE='Coef1', /ALIGN_LEFT)
; k1St = WIDGET_TEXT(main, VALUE=STRTRIM(STRING(material->getIK(1),/PRINT),2), XSIZE=10, /EDITABLE)
g1St = WIDGET_TEXT(main, VALUE=STRTRIM(STRING(material->getIG(1),/PRINT),2), XSIZE=10, /EDITABLE)
dummyLa = WIDGET_LABEL(main, VALUE='Coef2', /ALIGN_LEFT)
; k2St = WIDGET_TEXT(main, VALUE=STRTRIM(STRING(material->getIK(2),/PRINT),2), XSIZE=10, /EDITABLE)
g2St = WIDGET_TEXT(main, VALUE=STRTRIM(STRING(material->getIG(2),/PRINT),2), XSIZE=10, /EDITABLE)
; Buttons
buttons = WIDGET_BASE(input,/ROW, /ALIGN_CENTER, /GRID_LAYOUT)
ok = WIDGET_BUTTON(buttons, VALUE='Ok', UVALUE='OK')
cancel = WIDGET_BUTTON(buttons, VALUE='Cancel', UVALUE='CANCEL')
; Finishing up
stash = {input: input, g0St: g0St,  g1St: g1St, g2St: g2St}
WIDGET_CONTROL, input, SET_UVALUE=stash
WIDGET_CONTROL, input, /REALIZE
XMANAGER, 'chgElPropIsoWindow', input
END

; ********************************* Anisotropic elastic properties (cubic) dialog **********

PRO doChgElPropCubicAnis, input, c11St, c12St, c44St,  dc11St, dc12St, dc44St,   ddc11St,  ddc12St, ddc44St
common materialwindow, material
WIDGET_CONTROL, c11St, GET_VALUE=c11S
WIDGET_CONTROL, c12St, GET_VALUE=c12S
WIDGET_CONTROL, c44St, GET_VALUE=c44S
WIDGET_CONTROL, dc11St, GET_VALUE=dc11S
WIDGET_CONTROL, dc12St, GET_VALUE=dc12S
WIDGET_CONTROL, dc44St, GET_VALUE=dc44S
WIDGET_CONTROL, ddc11St, GET_VALUE=ddc11S
WIDGET_CONTROL, ddc12St, GET_VALUE=ddc12S
WIDGET_CONTROL, ddc44St, GET_VALUE=ddc44S
c11 = float(c11S)
c12 = float(c12S)
c44 = float(c44S)
dc11 = float(dc11S)
dc12 = float(dc12S)
dc44 = float(dc44S)
ddc11 = float(ddc11S)
ddc12 = float(ddc12S)
ddc44 = float(ddc44S)
material->setAnisPropCubic, c11, c12, c44,  dc11,  dc12,  dc44,   ddc11,  ddc12, ddc44
WIDGET_CONTROL, input, /DESTROY
END

PRO chgElPropCubicAnis_event, ev
; Get the 'stash' structure.
WIDGET_CONTROL, ev.TOP, GET_UVALUE=stash
WIDGET_CONTROL, ev.ID, GET_UVALUE=uval
CASE ev.id OF
	stash.input:
	else: begin
		CASE uval OF
		'OK': doChgElPropCubicAnis, stash.input, stash.c11St,  stash.c12St, stash.c44St,  stash.dc11St, stash.dc12St, stash.dc44St,   stash.ddc11St,  stash.ddc12St,  stash.ddc44St
		'CANCEL': WIDGET_CONTROL, stash.input, /DESTROY
		else:
		ENDCASE
	endcase
endcase
END

PRO chgElPropCubicAnis, base
common materialwindow, material
common fonts, titlefont, boldfont, mainfont
; base GUI
input = WIDGET_BASE(Title='Anisotropic elastic properties', /COLUMN, /MODAL, GROUP_LEADER=base)
inputLa = WIDGET_LABEL(input, VALUE='Anisotropic elastic properties', /ALIGN_CENTER, font=titlefont)
; Material properties
mat = WIDGET_BASE(input, /COLUMN, FRAME=1)
; Main properties
main = WIDGET_BASE(mat, COLUMN=4, /GRID_LAYOUT, FRAME=0)
dummyLa = WIDGET_LABEL(main, VALUE='', /ALIGN_LEFT)
c11La = WIDGET_LABEL(main, VALUE='C11', /ALIGN_LEFT)
c12La = WIDGET_LABEL(main, VALUE='C12', /ALIGN_LEFT)
c44La = WIDGET_LABEL(main, VALUE='C44', /ALIGN_LEFT)
dummyLa = WIDGET_LABEL(main, VALUE='Coef0', /ALIGN_LEFT)
c11St = WIDGET_TEXT(main, VALUE=STRTRIM(STRING(material->getCij(1,1,0),/PRINT),2), XSIZE=10, /EDITABLE)
c12St = WIDGET_TEXT(main, VALUE=STRTRIM(STRING(material->getCij(1,2,0),/PRINT),2), XSIZE=10, /EDITABLE)
c44St = WIDGET_TEXT(main, VALUE=STRTRIM(STRING(material->getCij(4,4,0),/PRINT),2), XSIZE=10, /EDITABLE)
dummyLa = WIDGET_LABEL(main, VALUE='Coef1', /ALIGN_LEFT)
dc11St = WIDGET_TEXT(main, VALUE=STRTRIM(STRING(material->getCij(1,1,1),/PRINT),2), XSIZE=10, /EDITABLE)
dc12St = WIDGET_TEXT(main, VALUE=STRTRIM(STRING(material->getCij(1,2,1),/PRINT),2), XSIZE=10, /EDITABLE)
dc44St = WIDGET_TEXT(main, VALUE=STRTRIM(STRING(material->getCij(4,4,1),/PRINT),2), XSIZE=10, /EDITABLE)
dummyLa = WIDGET_LABEL(main, VALUE='Coef2', /ALIGN_LEFT)
ddc11St = WIDGET_TEXT(main, VALUE=STRTRIM(STRING(material->getCij(1,1,2),/PRINT),2), XSIZE=10, /EDITABLE)
ddc12St = WIDGET_TEXT(main, VALUE=STRTRIM(STRING(material->getCij(1,2,2),/PRINT),2), XSIZE=10, /EDITABLE)
ddc44St = WIDGET_TEXT(main, VALUE=STRTRIM(STRING(material->getCij(4,4,2),/PRINT),2), XSIZE=10, /EDITABLE)
; Buttons
buttons = WIDGET_BASE(input,/ROW, /ALIGN_CENTER, /GRID_LAYOUT)
ok = WIDGET_BUTTON(buttons, VALUE='Ok', UVALUE='OK')
cancel = WIDGET_BUTTON(buttons, VALUE='Cancel', UVALUE='CANCEL')
; Finishing up
stash = {input: input, c11St: c11St, c12St: c12St, c44St: c44St, dc11St: dc11St, dc12St: dc12St, dc44St: dc44St, ddc11St: ddc11St, ddc12St: ddc12St, ddc44St: ddc44St}
WIDGET_CONTROL, input, SET_UVALUE=stash
WIDGET_CONTROL, input, /REALIZE
XMANAGER, 'chgElPropCubicAnis', input
END
; ********************************* Anisotropic elastic properties (trigonal) dialog **********
PRO doChgElPropTrigAnis, input, c11St,  c33St,  c12St,  c13St, c14St,  c15St, c44St,  dc11St,  dc33St,  dc12St,  dc13St, dc14St,  dc15St, dc44St,   ddc11St,  ddc33St,  ddc12St,  ddc13St, ddc14St,  ddc15St,  ddc44St
common materialwindow, material
WIDGET_CONTROL, c11St, GET_VALUE=c11S
WIDGET_CONTROL, c12St, GET_VALUE=c12S
WIDGET_CONTROL, c13St, GET_VALUE=c13S
WIDGET_CONTROL, c14St, GET_VALUE=c14S
WIDGET_CONTROL, c15St, GET_VALUE=c15S
WIDGET_CONTROL, c33St, GET_VALUE=c33S
WIDGET_CONTROL, c44St, GET_VALUE=c44S
WIDGET_CONTROL, dc11St, GET_VALUE=dc11S
WIDGET_CONTROL, dc12St, GET_VALUE=dc12S
WIDGET_CONTROL, dc13St, GET_VALUE=dc13S
WIDGET_CONTROL, dc14St, GET_VALUE=dc14S
WIDGET_CONTROL, dc15St, GET_VALUE=dc15S
WIDGET_CONTROL, dc33St, GET_VALUE=dc33S
WIDGET_CONTROL, dc44St, GET_VALUE=dc44S
WIDGET_CONTROL, ddc11St, GET_VALUE=ddc11S
WIDGET_CONTROL, ddc12St, GET_VALUE=ddc12S
WIDGET_CONTROL, ddc13St, GET_VALUE=ddc13S
WIDGET_CONTROL, ddc14St, GET_VALUE=ddc14S
WIDGET_CONTROL, ddc15St, GET_VALUE=ddc15S
WIDGET_CONTROL, ddc33St, GET_VALUE=ddc33S
WIDGET_CONTROL, ddc44St, GET_VALUE=ddc44S
c11 = float(c11S)
c12 = float(c12S)
c13 = float(c13S)
c14 = float(c14S)
c15 = float(c15S)
c33 = float(c33S)
c44 = float(c44S)
dc11 = float(dc11S)
dc12 = float(dc12S)
dc13 = float(dc13S)
dc14 = float(dc14S)
dc15 = float(dc15S)
dc33 = float(dc33S)
dc44 = float(dc44S)
ddc11 = float(ddc11S)
ddc12 = float(ddc12S)
ddc13 = float(ddc13S)
ddc14 = float(ddc14S)
ddc15 = float(ddc15S)
ddc33 = float(ddc33S)
ddc44 = float(ddc44S)
material->setAnisPropTrig, c11,  c33,  c12,  c13, c14, c15, c44,  dc11,  dc33,  dc12,  dc13, dc14,  dc15,  dc44,   ddc11,  ddc33,  ddc12,  ddc13, ddc14,  ddc15,  ddc44
WIDGET_CONTROL, input, /DESTROY
END

PRO chgElPropTrigAnis_event, ev
; Get the 'stash' structure.
WIDGET_CONTROL, ev.TOP, GET_UVALUE=stash
WIDGET_CONTROL, ev.ID, GET_UVALUE=uval
CASE ev.id OF
  stash.input:
  else: begin
    CASE uval OF
    'OK': doChgElPropTrigAnis, stash.input, stash.c11St,  stash.c33St,  stash.c12St,  stash.c13St, stash.c14St,  stash.c15St,  stash.c44St,  stash.dc11St,  stash.dc33St,  stash.dc12St,  stash.dc13St,  stash.dc14St,  stash.dc15St,  stash.dc44St,   stash.ddc11St,  stash.ddc33St,  stash.ddc12St,  stash.ddc13St, stash.ddc14St,  stash.ddc15St,  stash.ddc44St
    'CANCEL': WIDGET_CONTROL, stash.input, /DESTROY
    else:
    ENDCASE
  endcase
endcase
END

PRO chgElPropTrigAnis, base
common materialwindow, material
common fonts, titlefont, boldfont, mainfont
; base GUI
input = WIDGET_BASE(Title='Anisotropic elastic properties', /COLUMN, /MODAL, GROUP_LEADER=base)
inputLa = WIDGET_LABEL(input, VALUE='Anisotropic elastic properties', /ALIGN_CENTER, font=titlefont)
; Material properties
mat = WIDGET_BASE(input, /COLUMN, FRAME=1)
; Main properties
main = WIDGET_BASE(mat, COLUMN=4, /GRID_LAYOUT, FRAME=0)
dummyLa = WIDGET_LABEL(main, VALUE='', /ALIGN_LEFT)
c11La = WIDGET_LABEL(main, VALUE='C11', /ALIGN_LEFT)
c33La = WIDGET_LABEL(main, VALUE='C33', /ALIGN_LEFT)
c12La = WIDGET_LABEL(main, VALUE='C12', /ALIGN_LEFT)
c13La = WIDGET_LABEL(main, VALUE='C13', /ALIGN_LEFT)
c14La = WIDGET_LABEL(main, VALUE='C14', /ALIGN_LEFT)
c15La = WIDGET_LABEL(main, VALUE='C15', /ALIGN_LEFT)
c44La = WIDGET_LABEL(main, VALUE='C44', /ALIGN_LEFT)
dummyLa = WIDGET_LABEL(main, VALUE='Coef0', /ALIGN_LEFT)
c11St = WIDGET_TEXT(main, VALUE=STRTRIM(STRING(material->getCij(1,1,0),/PRINT),2), XSIZE=10, /EDITABLE)
c33St = WIDGET_TEXT(main, VALUE=STRTRIM(STRING(material->getCij(3,3,0),/PRINT),2), XSIZE=10, /EDITABLE)
c12St = WIDGET_TEXT(main, VALUE=STRTRIM(STRING(material->getCij(1,2,0),/PRINT),2), XSIZE=10, /EDITABLE)
c13St = WIDGET_TEXT(main, VALUE=STRTRIM(STRING(material->getCij(1,3,0),/PRINT),2), XSIZE=10, /EDITABLE)
c14St = WIDGET_TEXT(main, VALUE=STRTRIM(STRING(material->getCij(1,4,0),/PRINT),2), XSIZE=10, /EDITABLE)
c15St = WIDGET_TEXT(main, VALUE=STRTRIM(STRING(material->getCij(1,5,0),/PRINT),2), XSIZE=10, /EDITABLE)
c44St = WIDGET_TEXT(main, VALUE=STRTRIM(STRING(material->getCij(4,4,0),/PRINT),2), XSIZE=10, /EDITABLE)
dummyLa = WIDGET_LABEL(main, VALUE='Coef1', /ALIGN_LEFT)
dc11St = WIDGET_TEXT(main, VALUE=STRTRIM(STRING(material->getCij(1,1,1),/PRINT),2), XSIZE=10, /EDITABLE)
dc33St = WIDGET_TEXT(main, VALUE=STRTRIM(STRING(material->getCij(3,3,1),/PRINT),2), XSIZE=10, /EDITABLE)
dc12St = WIDGET_TEXT(main, VALUE=STRTRIM(STRING(material->getCij(1,2,1),/PRINT),2), XSIZE=10, /EDITABLE)
dc13St = WIDGET_TEXT(main, VALUE=STRTRIM(STRING(material->getCij(1,3,1),/PRINT),2), XSIZE=10, /EDITABLE)
dc14St = WIDGET_TEXT(main, VALUE=STRTRIM(STRING(material->getCij(1,4,1),/PRINT),2), XSIZE=10, /EDITABLE)
dc15St = WIDGET_TEXT(main, VALUE=STRTRIM(STRING(material->getCij(1,5,1),/PRINT),2), XSIZE=10, /EDITABLE)
dc44St = WIDGET_TEXT(main, VALUE=STRTRIM(STRING(material->getCij(4,4,1),/PRINT),2), XSIZE=10, /EDITABLE)
dummyLa = WIDGET_LABEL(main, VALUE='Coef2', /ALIGN_LEFT)
ddc11St = WIDGET_TEXT(main, VALUE=STRTRIM(STRING(material->getCij(1,1,2),/PRINT),2), XSIZE=10, /EDITABLE)
ddc33St = WIDGET_TEXT(main, VALUE=STRTRIM(STRING(material->getCij(3,3,2),/PRINT),2), XSIZE=10, /EDITABLE)
ddc12St = WIDGET_TEXT(main, VALUE=STRTRIM(STRING(material->getCij(1,2,2),/PRINT),2), XSIZE=10, /EDITABLE)
ddc13St = WIDGET_TEXT(main, VALUE=STRTRIM(STRING(material->getCij(1,3,2),/PRINT),2), XSIZE=10, /EDITABLE)
ddc14St = WIDGET_TEXT(main, VALUE=STRTRIM(STRING(material->getCij(1,4,2),/PRINT),2), XSIZE=10, /EDITABLE)
ddc15St = WIDGET_TEXT(main, VALUE=STRTRIM(STRING(material->getCij(1,5,2),/PRINT),2), XSIZE=10, /EDITABLE)
ddc44St = WIDGET_TEXT(main, VALUE=STRTRIM(STRING(material->getCij(4,4,2),/PRINT),2), XSIZE=10, /EDITABLE)
; Buttons
buttons = WIDGET_BASE(input,/ROW, /ALIGN_CENTER, /GRID_LAYOUT)
ok = WIDGET_BUTTON(buttons, VALUE='Ok', UVALUE='OK')
cancel = WIDGET_BUTTON(buttons, VALUE='Cancel', UVALUE='CANCEL')
; Finishing up
stash = {input: input, c11St: c11St, c12St: c12St, c13St: c13St, c14St: c14St, c15St: c15St, c33St: c33St, c44St: c44St, dc11St: dc11St, dc12St: dc12St, dc13St: dc13St, dc14St: dc14St, dc15St: dc15St, dc33St: dc33St, dc44St: dc44St, ddc11St: ddc11St, ddc12St: ddc12St, ddc13St: ddc13St, ddc14St: ddc14St, ddc15St: ddc15St, ddc33St: ddc33St, ddc44St: ddc44St}
WIDGET_CONTROL, input, SET_UVALUE=stash
WIDGET_CONTROL, input, /REALIZE
XMANAGER, 'chgElPropTrigAnis', input
END



; ********************************* Anisotropic elastic properties (hexagonal) dialog **********

PRO doChgElPropHexaAnis, input, c11St,  c33St,  c12St,  c13St,  c44St,  dc11St,  dc33St,  dc12St,  dc13St,  dc44St,   ddc11St,  ddc33St,  ddc12St,  ddc13St,  ddc44St
common materialwindow, material
WIDGET_CONTROL, c11St, GET_VALUE=c11S
WIDGET_CONTROL, c12St, GET_VALUE=c12S
WIDGET_CONTROL, c13St, GET_VALUE=c13S
WIDGET_CONTROL, c33St, GET_VALUE=c33S
WIDGET_CONTROL, c44St, GET_VALUE=c44S
WIDGET_CONTROL, dc11St, GET_VALUE=dc11S
WIDGET_CONTROL, dc12St, GET_VALUE=dc12S
WIDGET_CONTROL, dc13St, GET_VALUE=dc13S
WIDGET_CONTROL, dc33St, GET_VALUE=dc33S
WIDGET_CONTROL, dc44St, GET_VALUE=dc44S
WIDGET_CONTROL, ddc11St, GET_VALUE=ddc11S
WIDGET_CONTROL, ddc12St, GET_VALUE=ddc12S
WIDGET_CONTROL, ddc13St, GET_VALUE=ddc13S
WIDGET_CONTROL, ddc33St, GET_VALUE=ddc33S
WIDGET_CONTROL, ddc44St, GET_VALUE=ddc44S
c11 = float(c11S)
c12 = float(c12S)
c13 = float(c13S)
c33 = float(c33S)
c44 = float(c44S)
dc11 = float(dc11S)
dc12 = float(dc12S)
dc13 = float(dc13S)
dc33 = float(dc33S)
dc44 = float(dc44S)
ddc11 = float(ddc11S)
ddc12 = float(ddc12S)
ddc13 = float(ddc13S)
ddc33 = float(ddc33S)
ddc44 = float(ddc44S)
material->setAnisPropHexa, c11,  c33,  c12,  c13,  c44,  dc11,  dc33,  dc12,  dc13,  dc44,   ddc11,  ddc33,  ddc12,  ddc13,  ddc44
WIDGET_CONTROL, input, /DESTROY
END

PRO chgElPropHexaAnis_event, ev
; Get the 'stash' structure.
WIDGET_CONTROL, ev.TOP, GET_UVALUE=stash
WIDGET_CONTROL, ev.ID, GET_UVALUE=uval
CASE ev.id OF
	stash.input:
	else: begin
		CASE uval OF
		'OK': doChgElPropHexaAnis, stash.input, stash.c11St,  stash.c33St,  stash.c12St,  stash.c13St,  stash.c44St,  stash.dc11St,  stash.dc33St,  stash.dc12St,  stash.dc13St,  stash.dc44St,   stash.ddc11St,  stash.ddc33St,  stash.ddc12St,  stash.ddc13St,  stash.ddc44St
		'CANCEL': WIDGET_CONTROL, stash.input, /DESTROY
		else:
		ENDCASE
	endcase
endcase
END

PRO chgElPropHexaAnis, base
common materialwindow, material
common fonts, titlefont, boldfont, mainfont
; base GUI
input = WIDGET_BASE(Title='Anisotropic elastic properties', /COLUMN, /MODAL, GROUP_LEADER=base)
inputLa = WIDGET_LABEL(input, VALUE='Anisotropic elastic properties', /ALIGN_CENTER, font=titlefont)
; Material properties
mat = WIDGET_BASE(input, /COLUMN, FRAME=1)
; Main properties
main = WIDGET_BASE(mat, COLUMN=4, /GRID_LAYOUT, FRAME=0)
dummyLa = WIDGET_LABEL(main, VALUE='', /ALIGN_LEFT)
c11La = WIDGET_LABEL(main, VALUE='C11', /ALIGN_LEFT)
c33La = WIDGET_LABEL(main, VALUE='C33', /ALIGN_LEFT)
c12La = WIDGET_LABEL(main, VALUE='C12', /ALIGN_LEFT)
c13La = WIDGET_LABEL(main, VALUE='C13', /ALIGN_LEFT)
c44La = WIDGET_LABEL(main, VALUE='C44', /ALIGN_LEFT)
dummyLa = WIDGET_LABEL(main, VALUE='Coef0', /ALIGN_LEFT)
c11St = WIDGET_TEXT(main, VALUE=STRTRIM(STRING(material->getCij(1,1,0),/PRINT),2), XSIZE=10, /EDITABLE)
c33St = WIDGET_TEXT(main, VALUE=STRTRIM(STRING(material->getCij(3,3,0),/PRINT),2), XSIZE=10, /EDITABLE)
c12St = WIDGET_TEXT(main, VALUE=STRTRIM(STRING(material->getCij(1,2,0),/PRINT),2), XSIZE=10, /EDITABLE)
c13St = WIDGET_TEXT(main, VALUE=STRTRIM(STRING(material->getCij(1,3,0),/PRINT),2), XSIZE=10, /EDITABLE)
c44St = WIDGET_TEXT(main, VALUE=STRTRIM(STRING(material->getCij(4,4,0),/PRINT),2), XSIZE=10, /EDITABLE)
dummyLa = WIDGET_LABEL(main, VALUE='Coef1', /ALIGN_LEFT)
dc11St = WIDGET_TEXT(main, VALUE=STRTRIM(STRING(material->getCij(1,1,1),/PRINT),2), XSIZE=10, /EDITABLE)
dc33St = WIDGET_TEXT(main, VALUE=STRTRIM(STRING(material->getCij(3,3,1),/PRINT),2), XSIZE=10, /EDITABLE)
dc12St = WIDGET_TEXT(main, VALUE=STRTRIM(STRING(material->getCij(1,2,1),/PRINT),2), XSIZE=10, /EDITABLE)
dc13St = WIDGET_TEXT(main, VALUE=STRTRIM(STRING(material->getCij(1,3,1),/PRINT),2), XSIZE=10, /EDITABLE)
dc44St = WIDGET_TEXT(main, VALUE=STRTRIM(STRING(material->getCij(4,4,1),/PRINT),2), XSIZE=10, /EDITABLE)
dummyLa = WIDGET_LABEL(main, VALUE='Coef2', /ALIGN_LEFT)
ddc11St = WIDGET_TEXT(main, VALUE=STRTRIM(STRING(material->getCij(1,1,2),/PRINT),2), XSIZE=10, /EDITABLE)
ddc33St = WIDGET_TEXT(main, VALUE=STRTRIM(STRING(material->getCij(3,3,2),/PRINT),2), XSIZE=10, /EDITABLE)
ddc12St = WIDGET_TEXT(main, VALUE=STRTRIM(STRING(material->getCij(1,2,2),/PRINT),2), XSIZE=10, /EDITABLE)
ddc13St = WIDGET_TEXT(main, VALUE=STRTRIM(STRING(material->getCij(1,3,2),/PRINT),2), XSIZE=10, /EDITABLE)
ddc44St = WIDGET_TEXT(main, VALUE=STRTRIM(STRING(material->getCij(4,4,2),/PRINT),2), XSIZE=10, /EDITABLE)
; Buttons
buttons = WIDGET_BASE(input,/ROW, /ALIGN_CENTER, /GRID_LAYOUT)
ok = WIDGET_BUTTON(buttons, VALUE='Ok', UVALUE='OK')
cancel = WIDGET_BUTTON(buttons, VALUE='Cancel', UVALUE='CANCEL')
; Finishing up
stash = {input: input, c11St: c11St, c12St: c12St, c13St: c13St, c33St: c33St, c44St: c44St, dc11St: dc11St, dc12St: dc12St, dc13St: dc13St, dc33St: dc33St, dc44St: dc44St, ddc11St: ddc11St, ddc12St: ddc12St, ddc13St: ddc13St, ddc33St: ddc33St, ddc44St: ddc44St}
WIDGET_CONTROL, input, SET_UVALUE=stash
WIDGET_CONTROL, input, /REALIZE
XMANAGER, 'chgElPropHexaAnis', input
END



; ********************************* Anisotropic elastic properties (orthorhombic) dialog **********

PRO doChgElPropOrthoAnis, input, stash
common materialwindow, material
WIDGET_CONTROL, stash.c11St, GET_VALUE=c11S
WIDGET_CONTROL, stash.c22St, GET_VALUE=c22S
WIDGET_CONTROL, stash.c33St, GET_VALUE=c33S
WIDGET_CONTROL, stash.c12St, GET_VALUE=c12S
WIDGET_CONTROL, stash.c13St, GET_VALUE=c13S
WIDGET_CONTROL, stash.c23St, GET_VALUE=c23S
WIDGET_CONTROL, stash.c44St, GET_VALUE=c44S
WIDGET_CONTROL, stash.c55St, GET_VALUE=c55S
WIDGET_CONTROL, stash.c66St, GET_VALUE=c66S
WIDGET_CONTROL, stash.dc11St, GET_VALUE=dc11S
WIDGET_CONTROL, stash.dc22St, GET_VALUE=dc22S
WIDGET_CONTROL, stash.dc33St, GET_VALUE=dc33S
WIDGET_CONTROL, stash.dc12St, GET_VALUE=dc12S
WIDGET_CONTROL, stash.dc13St, GET_VALUE=dc13S
WIDGET_CONTROL, stash.dc23St, GET_VALUE=dc23S
WIDGET_CONTROL, stash.dc44St, GET_VALUE=dc44S
WIDGET_CONTROL, stash.dc55St, GET_VALUE=dc55S
WIDGET_CONTROL, stash.dc66St, GET_VALUE=dc66S
WIDGET_CONTROL, stash.ddc11St, GET_VALUE=ddc11S
WIDGET_CONTROL, stash.ddc22St, GET_VALUE=ddc22S
WIDGET_CONTROL, stash.ddc33St, GET_VALUE=ddc33S
WIDGET_CONTROL, stash.ddc12St, GET_VALUE=ddc12S
WIDGET_CONTROL, stash.ddc13St, GET_VALUE=ddc13S
WIDGET_CONTROL, stash.ddc23St, GET_VALUE=ddc23S
WIDGET_CONTROL, stash.ddc44St, GET_VALUE=ddc44S
WIDGET_CONTROL, stash.ddc55St, GET_VALUE=ddc55S
WIDGET_CONTROL, stash.ddc66St, GET_VALUE=ddc66S
c11 = float(c11S)
c22 = float(c22S)
c33 = float(c33S)
c12 = float(c12S)
c13 = float(c13S)
c23 = float(c23S)
c44 = float(c44S)
c55 = float(c55S)
c66 = float(c66S)
dc11 = float(dc11S)
dc22 = float(dc22S)
dc33 = float(dc33S)
dc12 = float(dc12S)
dc13 = float(dc13S)
dc23 = float(dc23S)
dc44 = float(dc44S)
dc55 = float(dc55S)
dc66 = float(dc66S)
ddc11 = float(ddc11S)
ddc22 = float(ddc22S)
ddc33 = float(ddc33S)
ddc12 = float(ddc12S)
ddc13 = float(ddc13S)
ddc23 = float(ddc23S)
ddc44 = float(ddc44S)
ddc55 = float(ddc55S)
ddc66 = float(ddc66S)

material->setAnisPropOrtho, c11,  c22, c33,  c12,  c13, c23, c44, c55, c66,  dc11, dc22, dc33,  dc12,  dc13, dc23, dc44, dc55, dc66,  ddc11, ddc22,  ddc33,  ddc12,  ddc13, ddc23,  ddc44, ddc55, ddc66
WIDGET_CONTROL, input, /DESTROY
END

PRO chgElPropOrthoAnis_event, ev
; Get the 'stash' structure.
WIDGET_CONTROL, ev.TOP, GET_UVALUE=stash
WIDGET_CONTROL, ev.ID, GET_UVALUE=uval
CASE ev.id OF
  stash.input:
  else: begin
    CASE uval OF
    'OK': doChgElPropOrthoAnis, stash.input, stash
    'CANCEL': WIDGET_CONTROL, stash.input, /DESTROY
    else:
    ENDCASE
  endcase
endcase
END

PRO chgElPropOrthoAnis, base
common materialwindow, material
common fonts, titlefont, boldfont, mainfont
; base GUI
input = WIDGET_BASE(Title='Anisotropic elastic properties', /COLUMN, /MODAL, GROUP_LEADER=base)
inputLa = WIDGET_LABEL(input, VALUE='Anisotropic elastic properties', /ALIGN_CENTER, font=titlefont)
; Material properties
mat = WIDGET_BASE(input, /COLUMN, FRAME=1)
; Main properties
main = WIDGET_BASE(mat, COLUMN=4, /GRID_LAYOUT, FRAME=0)
dummyLa = WIDGET_LABEL(main, VALUE='', /ALIGN_LEFT)
c11La = WIDGET_LABEL(main, VALUE='C11', /ALIGN_LEFT)
c22La = WIDGET_LABEL(main, VALUE='C22', /ALIGN_LEFT)
c33La = WIDGET_LABEL(main, VALUE='C33', /ALIGN_LEFT)
c12La = WIDGET_LABEL(main, VALUE='C12', /ALIGN_LEFT)
c13La = WIDGET_LABEL(main, VALUE='C13', /ALIGN_LEFT)
c23La = WIDGET_LABEL(main, VALUE='C23', /ALIGN_LEFT)
c44La = WIDGET_LABEL(main, VALUE='C44', /ALIGN_LEFT)
c55La = WIDGET_LABEL(main, VALUE='C55', /ALIGN_LEFT)
c66La = WIDGET_LABEL(main, VALUE='C66', /ALIGN_LEFT)
dummyLa = WIDGET_LABEL(main, VALUE='Coef0', /ALIGN_LEFT)
c11St = WIDGET_TEXT(main, VALUE=STRTRIM(STRING(material->getCij(1,1,0),/PRINT),2), XSIZE=10, /EDITABLE)
c22St = WIDGET_TEXT(main, VALUE=STRTRIM(STRING(material->getCij(2,2,0),/PRINT),2), XSIZE=10, /EDITABLE)
c33St = WIDGET_TEXT(main, VALUE=STRTRIM(STRING(material->getCij(3,3,0),/PRINT),2), XSIZE=10, /EDITABLE)
c12St = WIDGET_TEXT(main, VALUE=STRTRIM(STRING(material->getCij(1,2,0),/PRINT),2), XSIZE=10, /EDITABLE)
c13St = WIDGET_TEXT(main, VALUE=STRTRIM(STRING(material->getCij(1,3,0),/PRINT),2), XSIZE=10, /EDITABLE)
c23St = WIDGET_TEXT(main, VALUE=STRTRIM(STRING(material->getCij(2,3,0),/PRINT),2), XSIZE=10, /EDITABLE)
c44St = WIDGET_TEXT(main, VALUE=STRTRIM(STRING(material->getCij(4,4,0),/PRINT),2), XSIZE=10, /EDITABLE)
c55St = WIDGET_TEXT(main, VALUE=STRTRIM(STRING(material->getCij(5,5,0),/PRINT),2), XSIZE=10, /EDITABLE)
c66St = WIDGET_TEXT(main, VALUE=STRTRIM(STRING(material->getCij(6,6,0),/PRINT),2), XSIZE=10, /EDITABLE)
dummyLa = WIDGET_LABEL(main, VALUE='Coef1', /ALIGN_LEFT)
dc11St = WIDGET_TEXT(main, VALUE=STRTRIM(STRING(material->getCij(1,1,1),/PRINT),2), XSIZE=10, /EDITABLE)
dc22St = WIDGET_TEXT(main, VALUE=STRTRIM(STRING(material->getCij(2,2,1),/PRINT),2), XSIZE=10, /EDITABLE)
dc33St = WIDGET_TEXT(main, VALUE=STRTRIM(STRING(material->getCij(3,3,1),/PRINT),2), XSIZE=10, /EDITABLE)
dc12St = WIDGET_TEXT(main, VALUE=STRTRIM(STRING(material->getCij(1,2,1),/PRINT),2), XSIZE=10, /EDITABLE)
dc13St = WIDGET_TEXT(main, VALUE=STRTRIM(STRING(material->getCij(1,3,1),/PRINT),2), XSIZE=10, /EDITABLE)
dc23St = WIDGET_TEXT(main, VALUE=STRTRIM(STRING(material->getCij(2,3,1),/PRINT),2), XSIZE=10, /EDITABLE)
dc44St = WIDGET_TEXT(main, VALUE=STRTRIM(STRING(material->getCij(4,4,1),/PRINT),2), XSIZE=10, /EDITABLE)
dc55St = WIDGET_TEXT(main, VALUE=STRTRIM(STRING(material->getCij(5,5,1),/PRINT),2), XSIZE=10, /EDITABLE)
dc66St = WIDGET_TEXT(main, VALUE=STRTRIM(STRING(material->getCij(6,6,1),/PRINT),2), XSIZE=10, /EDITABLE)
dummyLa = WIDGET_LABEL(main, VALUE='Coef2', /ALIGN_LEFT)
ddc11St = WIDGET_TEXT(main, VALUE=STRTRIM(STRING(material->getCij(1,1,2),/PRINT),2), XSIZE=10, /EDITABLE)
ddc22St = WIDGET_TEXT(main, VALUE=STRTRIM(STRING(material->getCij(2,2,2),/PRINT),2), XSIZE=10, /EDITABLE)
ddc33St = WIDGET_TEXT(main, VALUE=STRTRIM(STRING(material->getCij(3,3,2),/PRINT),2), XSIZE=10, /EDITABLE)
ddc12St = WIDGET_TEXT(main, VALUE=STRTRIM(STRING(material->getCij(1,2,2),/PRINT),2), XSIZE=10, /EDITABLE)
ddc13St = WIDGET_TEXT(main, VALUE=STRTRIM(STRING(material->getCij(1,3,2),/PRINT),2), XSIZE=10, /EDITABLE)
ddc23St = WIDGET_TEXT(main, VALUE=STRTRIM(STRING(material->getCij(2,3,2),/PRINT),2), XSIZE=10, /EDITABLE)
ddc44St = WIDGET_TEXT(main, VALUE=STRTRIM(STRING(material->getCij(4,4,2),/PRINT),2), XSIZE=10, /EDITABLE)
ddc55St = WIDGET_TEXT(main, VALUE=STRTRIM(STRING(material->getCij(5,5,2),/PRINT),2), XSIZE=10, /EDITABLE)
ddc66St = WIDGET_TEXT(main, VALUE=STRTRIM(STRING(material->getCij(6,6,2),/PRINT),2), XSIZE=10, /EDITABLE)
; Buttons
buttons = WIDGET_BASE(input,/ROW, /ALIGN_CENTER, /GRID_LAYOUT)
ok = WIDGET_BUTTON(buttons, VALUE='Ok', UVALUE='OK')
cancel = WIDGET_BUTTON(buttons, VALUE='Cancel', UVALUE='CANCEL')
; Finishing up
stash = {input: input, $
  c11St: c11St, c22St: c22St, c12St: c12St, c13St: c13St, c23St: c23St, c33St: c33St, c44St: c44St, c55St: c55St, c66St: c66St, $
  dc11St: dc11St, dc22St: dc22St, dc12St: dc12St, dc13St: dc13St, dc23St: dc23St, dc33St: dc33St, dc44St: dc44St, dc55St: dc55St, dc66St: dc66St, $
  ddc11St: ddc11St, ddc22St: ddc22St, ddc12St: ddc12St, ddc13St: ddc13St, ddc23St: ddc23St, ddc33St: ddc33St, ddc44St: ddc44St, ddc55St: ddc55St, ddc66St: ddc66St}
WIDGET_CONTROL, input, SET_UVALUE=stash
WIDGET_CONTROL, input, /REALIZE
XMANAGER, 'chgElPropOrthoAnis', input
END

; ********************************* Anisotropic elastic properties (monoclinic) dialog **********

PRO doChgElPropMonoAnis, input, stash
common materialwindow, material
WIDGET_CONTROL, stash.c11St, GET_VALUE=c11S
WIDGET_CONTROL, stash.c22St, GET_VALUE=c22S
WIDGET_CONTROL, stash.c33St, GET_VALUE=c33S
WIDGET_CONTROL, stash.c12St, GET_VALUE=c12S
WIDGET_CONTROL, stash.c13St, GET_VALUE=c13S
WIDGET_CONTROL, stash.c15St, GET_VALUE=c15S
WIDGET_CONTROL, stash.c23St, GET_VALUE=c23S
WIDGET_CONTROL, stash.c25St, GET_VALUE=c25S
WIDGET_CONTROL, stash.c35St, GET_VALUE=c35S
WIDGET_CONTROL, stash.c44St, GET_VALUE=c44S
WIDGET_CONTROL, stash.c46St, GET_VALUE=c46S
WIDGET_CONTROL, stash.c55St, GET_VALUE=c55S
WIDGET_CONTROL, stash.c66St, GET_VALUE=c66S
WIDGET_CONTROL, stash.dc11St, GET_VALUE=dc11S
WIDGET_CONTROL, stash.dc22St, GET_VALUE=dc22S
WIDGET_CONTROL, stash.dc33St, GET_VALUE=dc33S
WIDGET_CONTROL, stash.dc12St, GET_VALUE=dc12S
WIDGET_CONTROL, stash.dc13St, GET_VALUE=dc13S
WIDGET_CONTROL, stash.dc15St, GET_VALUE=dc15S
WIDGET_CONTROL, stash.dc23St, GET_VALUE=dc23S
WIDGET_CONTROL, stash.dc25St, GET_VALUE=dc25S
WIDGET_CONTROL, stash.dc35St, GET_VALUE=dc35S
WIDGET_CONTROL, stash.dc44St, GET_VALUE=dc44S
WIDGET_CONTROL, stash.dc46St, GET_VALUE=dc46S
WIDGET_CONTROL, stash.dc55St, GET_VALUE=dc55S
WIDGET_CONTROL, stash.dc66St, GET_VALUE=dc66S
WIDGET_CONTROL, stash.ddc11St, GET_VALUE=ddc11S
WIDGET_CONTROL, stash.ddc22St, GET_VALUE=ddc22S
WIDGET_CONTROL, stash.ddc33St, GET_VALUE=ddc33S
WIDGET_CONTROL, stash.ddc12St, GET_VALUE=ddc12S
WIDGET_CONTROL, stash.ddc13St, GET_VALUE=ddc13S
WIDGET_CONTROL, stash.ddc15St, GET_VALUE=ddc15S
WIDGET_CONTROL, stash.ddc23St, GET_VALUE=ddc23S
WIDGET_CONTROL, stash.ddc25St, GET_VALUE=ddc25S
WIDGET_CONTROL, stash.ddc35St, GET_VALUE=ddc35S
WIDGET_CONTROL, stash.ddc44St, GET_VALUE=ddc44S
WIDGET_CONTROL, stash.ddc46St, GET_VALUE=ddc46S
WIDGET_CONTROL, stash.ddc55St, GET_VALUE=ddc55S
WIDGET_CONTROL, stash.ddc66St, GET_VALUE=ddc66S
c11 = float(c11S)
c22 = float(c22S)
c33 = float(c33S)
c12 = float(c12S)
c13 = float(c13S)
c15 = float(c15S)
c23 = float(c23S)
c25 = float(c25S)
c35 = float(c35S)
c44 = float(c44S)
c46 = float(c46S)
c55 = float(c55S)
c66 = float(c66S)
dc11 = float(dc11S)
dc22 = float(dc22S)
dc33 = float(dc33S)
dc12 = float(dc12S)
dc13 = float(dc13S)
dc15 = float(dc15S)
dc23 = float(dc23S)
dc25 = float(dc25S)
dc35 = float(dc35S)
dc44 = float(dc44S)
dc46 = float(dc46S)
dc55 = float(dc55S)
dc66 = float(dc66S)
ddc11 = float(ddc11S)
ddc22 = float(ddc22S)
ddc33 = float(ddc33S)
ddc12 = float(ddc12S)
ddc13 = float(ddc13S)
ddc15 = float(ddc15S)
ddc23 = float(ddc23S)
ddc25 = float(ddc25S)
ddc35 = float(ddc35S)
ddc44 = float(ddc44S)
ddc46 = float(ddc46S)
ddc55 = float(ddc55S)
ddc66 = float(ddc66S)

material->setAnisPropMono, c11,  c22, c33,  c12,  c13, c15, c23, c25, c35, c44, c46, c55, c66,  dc11, dc22, dc33,  dc12,  dc13, dc15, dc23, dc25, dc35, dc44, dc46, dc55, dc66,  ddc11, ddc22,  ddc33,  ddc12,  ddc13, ddc15, ddc23, ddc25, ddc35, ddc44, ddc46, ddc55, ddc66
WIDGET_CONTROL, input, /DESTROY
END

PRO chgElPropMonoAnis_event, ev
; Get the 'stash' structure.
WIDGET_CONTROL, ev.TOP, GET_UVALUE=stash
WIDGET_CONTROL, ev.ID, GET_UVALUE=uval
CASE ev.id OF
  stash.input:
  else: begin
    CASE uval OF
    'OK': doChgElPropMonoAnis, stash.input, stash
    'CANCEL': WIDGET_CONTROL, stash.input, /DESTROY
    else:
    ENDCASE
  endcase
endcase
END

PRO chgElPropMonoAnis, base
common materialwindow, material
common fonts, titlefont, boldfont, mainfont
; base GUI
input = WIDGET_BASE(Title='Anisotropic elastic properties', /COLUMN, /MODAL, GROUP_LEADER=base)
inputLa = WIDGET_LABEL(input, VALUE='Anisotropic elastic properties', /ALIGN_CENTER, font=titlefont)
; Material properties
mat = WIDGET_BASE(input, /COLUMN, FRAME=1)
; Main properties
main = WIDGET_BASE(mat, COLUMN=4, /GRID_LAYOUT, FRAME=0)
dummyLa = WIDGET_LABEL(main, VALUE='', /ALIGN_LEFT)
c11La = WIDGET_LABEL(main, VALUE='C11', /ALIGN_LEFT)
c22La = WIDGET_LABEL(main, VALUE='C22', /ALIGN_LEFT)
c33La = WIDGET_LABEL(main, VALUE='C33', /ALIGN_LEFT)
c12La = WIDGET_LABEL(main, VALUE='C12', /ALIGN_LEFT)
c13La = WIDGET_LABEL(main, VALUE='C13', /ALIGN_LEFT)
c15La = WIDGET_LABEL(main, VALUE='C15', /ALIGN_LEFT)
c23La = WIDGET_LABEL(main, VALUE='C23', /ALIGN_LEFT)
c25La = WIDGET_LABEL(main, VALUE='C25', /ALIGN_LEFT)
c35La = WIDGET_LABEL(main, VALUE='C35', /ALIGN_LEFT)
c44La = WIDGET_LABEL(main, VALUE='C44', /ALIGN_LEFT)
c46La = WIDGET_LABEL(main, VALUE='C46', /ALIGN_LEFT)
c55La = WIDGET_LABEL(main, VALUE='C55', /ALIGN_LEFT)
c66La = WIDGET_LABEL(main, VALUE='C66', /ALIGN_LEFT)
dummyLa = WIDGET_LABEL(main, VALUE='Coef0', /ALIGN_LEFT)
c11St = WIDGET_TEXT(main, VALUE=STRTRIM(STRING(material->getCij(1,1,0),/PRINT),2), XSIZE=10, /EDITABLE)
c22St = WIDGET_TEXT(main, VALUE=STRTRIM(STRING(material->getCij(2,2,0),/PRINT),2), XSIZE=10, /EDITABLE)
c33St = WIDGET_TEXT(main, VALUE=STRTRIM(STRING(material->getCij(3,3,0),/PRINT),2), XSIZE=10, /EDITABLE)
c12St = WIDGET_TEXT(main, VALUE=STRTRIM(STRING(material->getCij(1,2,0),/PRINT),2), XSIZE=10, /EDITABLE)
c13St = WIDGET_TEXT(main, VALUE=STRTRIM(STRING(material->getCij(1,3,0),/PRINT),2), XSIZE=10, /EDITABLE)
c15St = WIDGET_TEXT(main, VALUE=STRTRIM(STRING(material->getCij(1,5,0),/PRINT),2), XSIZE=10, /EDITABLE)
c23St = WIDGET_TEXT(main, VALUE=STRTRIM(STRING(material->getCij(2,3,0),/PRINT),2), XSIZE=10, /EDITABLE)
c25St = WIDGET_TEXT(main, VALUE=STRTRIM(STRING(material->getCij(2,5,0),/PRINT),2), XSIZE=10, /EDITABLE)
c35St = WIDGET_TEXT(main, VALUE=STRTRIM(STRING(material->getCij(3,5,0),/PRINT),2), XSIZE=10, /EDITABLE)
c44St = WIDGET_TEXT(main, VALUE=STRTRIM(STRING(material->getCij(4,4,0),/PRINT),2), XSIZE=10, /EDITABLE)
c46St = WIDGET_TEXT(main, VALUE=STRTRIM(STRING(material->getCij(4,6,0),/PRINT),2), XSIZE=10, /EDITABLE)
c55St = WIDGET_TEXT(main, VALUE=STRTRIM(STRING(material->getCij(5,5,0),/PRINT),2), XSIZE=10, /EDITABLE)
c66St = WIDGET_TEXT(main, VALUE=STRTRIM(STRING(material->getCij(6,6,0),/PRINT),2), XSIZE=10, /EDITABLE)
dummyLa = WIDGET_LABEL(main, VALUE='Coef1', /ALIGN_LEFT)
dc11St = WIDGET_TEXT(main, VALUE=STRTRIM(STRING(material->getCij(1,1,1),/PRINT),2), XSIZE=10, /EDITABLE)
dc22St = WIDGET_TEXT(main, VALUE=STRTRIM(STRING(material->getCij(2,2,1),/PRINT),2), XSIZE=10, /EDITABLE)
dc33St = WIDGET_TEXT(main, VALUE=STRTRIM(STRING(material->getCij(3,3,1),/PRINT),2), XSIZE=10, /EDITABLE)
dc12St = WIDGET_TEXT(main, VALUE=STRTRIM(STRING(material->getCij(1,2,1),/PRINT),2), XSIZE=10, /EDITABLE)
dc13St = WIDGET_TEXT(main, VALUE=STRTRIM(STRING(material->getCij(1,3,1),/PRINT),2), XSIZE=10, /EDITABLE)
dc15St = WIDGET_TEXT(main, VALUE=STRTRIM(STRING(material->getCij(1,5,1),/PRINT),2), XSIZE=10, /EDITABLE)
dc23St = WIDGET_TEXT(main, VALUE=STRTRIM(STRING(material->getCij(2,3,1),/PRINT),2), XSIZE=10, /EDITABLE)
dc25St = WIDGET_TEXT(main, VALUE=STRTRIM(STRING(material->getCij(2,5,1),/PRINT),2), XSIZE=10, /EDITABLE)
dc35St = WIDGET_TEXT(main, VALUE=STRTRIM(STRING(material->getCij(3,5,1),/PRINT),2), XSIZE=10, /EDITABLE)
dc44St = WIDGET_TEXT(main, VALUE=STRTRIM(STRING(material->getCij(4,4,1),/PRINT),2), XSIZE=10, /EDITABLE)
dc46St = WIDGET_TEXT(main, VALUE=STRTRIM(STRING(material->getCij(4,6,1),/PRINT),2), XSIZE=10, /EDITABLE)
dc55St = WIDGET_TEXT(main, VALUE=STRTRIM(STRING(material->getCij(5,5,1),/PRINT),2), XSIZE=10, /EDITABLE)
dc66St = WIDGET_TEXT(main, VALUE=STRTRIM(STRING(material->getCij(6,6,1),/PRINT),2), XSIZE=10, /EDITABLE)
dummyLa = WIDGET_LABEL(main, VALUE='Coef2', /ALIGN_LEFT)
ddc11St = WIDGET_TEXT(main, VALUE=STRTRIM(STRING(material->getCij(1,1,2),/PRINT),2), XSIZE=10, /EDITABLE)
ddc22St = WIDGET_TEXT(main, VALUE=STRTRIM(STRING(material->getCij(2,2,2),/PRINT),2), XSIZE=10, /EDITABLE)
ddc33St = WIDGET_TEXT(main, VALUE=STRTRIM(STRING(material->getCij(3,3,2),/PRINT),2), XSIZE=10, /EDITABLE)
ddc12St = WIDGET_TEXT(main, VALUE=STRTRIM(STRING(material->getCij(1,2,2),/PRINT),2), XSIZE=10, /EDITABLE)
ddc13St = WIDGET_TEXT(main, VALUE=STRTRIM(STRING(material->getCij(1,3,2),/PRINT),2), XSIZE=10, /EDITABLE)
ddc15St = WIDGET_TEXT(main, VALUE=STRTRIM(STRING(material->getCij(1,5,2),/PRINT),2), XSIZE=10, /EDITABLE)
ddc23St = WIDGET_TEXT(main, VALUE=STRTRIM(STRING(material->getCij(2,3,2),/PRINT),2), XSIZE=10, /EDITABLE)
ddc25St = WIDGET_TEXT(main, VALUE=STRTRIM(STRING(material->getCij(2,5,2),/PRINT),2), XSIZE=10, /EDITABLE)
ddc35St = WIDGET_TEXT(main, VALUE=STRTRIM(STRING(material->getCij(3,5,2),/PRINT),2), XSIZE=10, /EDITABLE)
ddc44St = WIDGET_TEXT(main, VALUE=STRTRIM(STRING(material->getCij(4,4,2),/PRINT),2), XSIZE=10, /EDITABLE)
ddc46St = WIDGET_TEXT(main, VALUE=STRTRIM(STRING(material->getCij(4,6,2),/PRINT),2), XSIZE=10, /EDITABLE)
ddc55St = WIDGET_TEXT(main, VALUE=STRTRIM(STRING(material->getCij(5,5,2),/PRINT),2), XSIZE=10, /EDITABLE)
ddc66St = WIDGET_TEXT(main, VALUE=STRTRIM(STRING(material->getCij(6,6,2),/PRINT),2), XSIZE=10, /EDITABLE)
; Buttons
buttons = WIDGET_BASE(input,/ROW, /ALIGN_CENTER, /GRID_LAYOUT)
ok = WIDGET_BUTTON(buttons, VALUE='Ok', UVALUE='OK')
cancel = WIDGET_BUTTON(buttons, VALUE='Cancel', UVALUE='CANCEL')
; Finishing up
stash = {input: input, $
  c11St: c11St, c22St: c22St, c12St: c12St, c13St: c13St, c15St: c15St,c23St: c23St,c25St: c25St, c33St: c33St, c35St: c35St, c44St: c44St, c46St: c46St, c55St: c55St, c66St: c66St, $
  dc11St: dc11St, dc22St: dc22St, dc12St: dc12St, dc13St: dc13St,dc15St: dc15St, dc23St: dc23St,dc25St: dc25St, dc33St: dc33St, dc35St: dc35St, dc44St: dc44St, dc46St: dc46St, dc55St: dc55St, dc66St: dc66St, $
  ddc11St: ddc11St, ddc22St: ddc22St, ddc12St: ddc12St, ddc13St: ddc13St,ddc15St: ddc15St, ddc23St: ddc23St, ddc25St: ddc25St, ddc33St: ddc33St, ddc35St: ddc35St, ddc44St: ddc44St, ddc46St: ddc46St, ddc55St: ddc55St, ddc66St: ddc66St}
WIDGET_CONTROL, input, SET_UVALUE=stash
WIDGET_CONTROL, input, /REALIZE
XMANAGER, 'chgElPropMonoAnis', input
END


; ******************************************************************** Main dialog *************

PRO chgElasticProp, base, elasticSt, symSt
common materialwindow, material
symCode = WIDGET_INFO(symSt, /DROPLIST_SELECT)
elasticModel = WIDGET_INFO(elasticSt, /DROPLIST_SELECT)
sym = material->getSymmetryFromCode(symCode)
if (elasticModel eq 0) then begin
	chgElPropIsoWindow, base
endif else begin
	if (sym eq 'hexa') then chgElPropHexaAnis, base
	if (sym eq 'cubic') then chgElPropCubicAnis, base
  if (sym eq 'ortho') then chgElPropOrthoAnis, base
  if (sym eq 'trig') then chgElPropTrigAnis, base
  if (sym eq 'mono') then chgElPropMonoAnis, base
endelse
END

PRO saveMatProp, input, elasticSt, symSt, nameSt, voSt, koSt, dKoSt
common materialwindow, material
symCode = WIDGET_INFO(symSt, /DROPLIST_SELECT)
elasticModel = WIDGET_INFO(elasticSt, /DROPLIST_SELECT)
WIDGET_CONTROL, nameSt, GET_VALUE=name
WIDGET_CONTROL, voSt, GET_VALUE=voS
WIDGET_CONTROL, koSt, GET_VALUE=koS
WIDGET_CONTROL, dkoSt, GET_VALUE=dkoS
vo = float(voS)
ko = float(koS)
dko = float(dkoS)
material->setSymmetryFromCode, symCode
material->setElasticModel, elasticModel
material->setName, name
material->setEOSParameters, vo, ko, dko
material->setTmp, 1
WIDGET_CONTROL, input, /DESTROY
END

pro updateElasticChoices, symSt, elasticSt
symCode = WIDGET_INFO(symSt, /DROPLIST_SELECT)
if (symCode gt 4) then begin
  WIDGET_CONTROL, elasticSt, SET_DROPLIST_SELECT=0
  WIDGET_CONTROL, elasticSt, sensitive=0
endif else begin
  WIDGET_CONTROL, elasticSt, sensitive=1
endelse
end

PRO materialWindow_event, ev
; Get the 'stash' structure.
WIDGET_CONTROL, ev.TOP, GET_UVALUE=stash
WIDGET_CONTROL, ev.ID, GET_UVALUE=uval
CASE ev.id OF
	stash.input:
	stash.symSt: updateElasticChoices, stash.symSt, stash.elasticSt
	stash.elasticSt:
	else: begin
		CASE uval OF
		'ELPROP': chgElasticProp, stash.input, stash.elasticSt, stash.symSt
		'OK': saveMatProp, stash.input, stash.elasticSt, stash.symSt, stash.nameSt, stash.voSt, stash.koSt, stash.dkoSt
		'CANCEL': WIDGET_CONTROL, stash.input, /DESTROY
		else:
		ENDCASE
	endcase
endcase
END

function materialWindow, theMat, base
common materialwindow, material
common fonts, titlefont, boldfont, mainfont
material = theMat
material->setTmp, 0
; base GUI
input = WIDGET_BASE(Title='Material properties', /COLUMN, /MODAL, GROUP_LEADER=base)
inputLa = WIDGET_LABEL(input, VALUE='Material properties', /ALIGN_CENTER, font=titlefont)
; Material properties
mat = WIDGET_BASE(input, /COLUMN, FRAME=1)
; Main properties
main = WIDGET_BASE(mat, COLUMN=2, /GRID_LAYOUT, FRAME=0)
nameLa = WIDGET_LABEL(main, VALUE='Name', /ALIGN_LEFT)
symLa = WIDGET_LABEL(main, VALUE='Symmetry', /ALIGN_LEFT)
voLa = WIDGET_LABEL(main, VALUE='V0', /ALIGN_LEFT)
koLa = WIDGET_LABEL(main, VALUE='K0', /ALIGN_LEFT)
dkoLa = WIDGET_LABEL(main, VALUE="K'0", /ALIGN_LEFT)
nameSt = WIDGET_TEXT(main, VALUE=material->getName(), XSIZE=10, /EDITABLE)
symList = ["cubic","hexagonal","orthorhombic","trigonal","monoclinic"]
symSt = WIDGET_DROPLIST(main, VALUE=symList)
select  = fix(material->getSymmetryCode())
WIDGET_CONTROL, symSt, SET_DROPLIST_SELECT=select
voSt = WIDGET_TEXT(main, VALUE=STRTRIM(STRING(material->getV0(),/PRINT),2), XSIZE=10, /EDITABLE)
koSt = WIDGET_TEXT(main, VALUE=STRTRIM(STRING(material->getK0(),/PRINT),2), XSIZE=10, /EDITABLE)
dkoSt = WIDGET_TEXT(main, VALUE=STRTRIM(STRING(material->getDK0(),/PRINT),2), XSIZE=10, /EDITABLE)
; Elastic model
elastic = WIDGET_BASE(mat, /ROW, FRAME=0)
elLa = WIDGET_LABEL(elastic, VALUE='Elastic model', /ALIGN_LEFT)
elList = ["isotropic","anisotropic"]
elasticSt = WIDGET_DROPLIST(elastic, VALUE=elList)
; symmetries other than cubic, hexagonal, trigonal and orthorhombic are always isotropic
if (fix(material->getSymmetryCode()) gt 10) then begin
  WIDGET_CONTROL, elasticSt, SET_DROPLIST_SELECT=0
  WIDGET_CONTROL, elasticSt, sensitive=0
endif else begin
  select =  fix(material->getElasticModelCode())
  WIDGET_CONTROL, elasticSt, SET_DROPLIST_SELECT=select
endelse
inputFilesDirChg = WIDGET_BUTTON(elastic, VALUE='Options', UVALUE='ELPROP')
; Buttons
buttons = WIDGET_BASE(input,/ROW, /ALIGN_CENTER, /GRID_LAYOUT)
ok = WIDGET_BUTTON(buttons, VALUE='Ok', UVALUE='OK')
cancel = WIDGET_BUTTON(buttons, VALUE='Cancel', UVALUE='CANCEL')
; Finishing up
stash = {input: input, symSt: symSt, voSt: voSt, nameSt: nameSt, koSt: koSt, dkoSt: dkoSt, elasticSt: elasticSt}
WIDGET_CONTROL, input, SET_UVALUE=stash
WIDGET_CONTROL, input, /REALIZE
XMANAGER, 'materialWindow', input
RETURN, material
end