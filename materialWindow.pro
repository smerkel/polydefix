
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
if (symCode gt 1) then begin
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
symList = ["cubic","hexagonal","orthorhombic"]
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
; symmetries other than cubic and hexagonal are always isotropic
if (fix(material->getSymmetryCode()) gt 1) then begin
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