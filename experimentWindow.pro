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

FORWARD_FUNCTION experimentWindow

; ****************************************** about window **************

PRO aboutWindow_event, ev
WIDGET_CONTROL, ev.TOP, /DESTROY
END

pro aboutWindow, base
common fonts, titlefont, boldfont
basedialog = WIDGET_BASE(/COLUMN, /MODAL, GROUP_LEADER=base, Title='About Polydefix')
infobase =  WIDGET_BASE(basedialog,/COLUMN)
la = WIDGET_LABEL(infobase, VALUE='Polydefix', /ALIGN_LEFT, font=titlefont)
la = WIDGET_LABEL(infobase, VALUE='', /ALIGN_LEFT)
la = WIDGET_LABEL(infobase, VALUE='Polydefix, Polycrystal Deformation using X-rays', /ALIGN_LEFT)
la = WIDGET_LABEL(infobase, VALUE='Build 7, 14 Oct 2011', /ALIGN_LEFT)
la = WIDGET_LABEL(infobase, VALUE='Copyright S. Merkel, Universite Lille 1, France', /ALIGN_LEFT)
la = WIDGET_LABEL(infobase, VALUE='http://merkel.ZoneO.net/Polydefix/', /ALIGN_LEFT)
la = WIDGET_LABEL(infobase, VALUE='', /ALIGN_LEFT)
buttons = WIDGET_BASE(basedialog,/ROW, /GRID_LAYOUT, /ALIGN_CENTER)
ok = WIDGET_BUTTON(buttons, VALUE='Ok', UVALUE='OK')
WIDGET_CONTROL, basedialog, /REALIZE
XMANAGER, 'aboutWindow', basedialog
end

; ************************************************************ setFitOptions ****************
; Functions to deal with offsets and fit options
;   new in 1.3, 01-2010

pro doChangeOptions, stash
common experimentwindow, set, experiment
widget_control, stash.bgroupOffset, GET_VALUE=fitoffset
widget_control, stash.bgroupCenter, GET_VALUE=fitcenter
WIDGET_CONTROL, stash.inputOffsetSt, GET_VALUE=offsetS
offset = FLOAT(offsetS[0])
experiment->setFitOffset, fitoffset
experiment->setFitCenter, fitcenter
experiment->setOffset, offset
WIDGET_CONTROL, stash.baseOffset, SET_VALUE=strtrim(string(experiment->getOffset()),2)
if (experiment->getFitOffset() eq 1) then begin
	WIDGET_CONTROL, stash.baseFitOffset, SET_VALUE='Yes'
endif else WIDGET_CONTROL, stash.baseFitOffset, SET_VALUE='No'
if (experiment->getFitCenter() eq 1) then begin
	WIDGET_CONTROL, stash.baseFitCenter, SET_VALUE='Yes'
endif else WIDGET_CONTROL, stash.baseFitCenter, SET_VALUE='No'
logit, stash.log, "Changed fitting options and offset angle...\n"
WIDGET_CONTROL, stash.input, /DESTROY
end

pro offsetFitModeChanged, stash
widget_control, stash.bgroupOffset, GET_VALUE=test
if (test eq 1) then widget_control, stash.bgroupCenter, SET_VALUE=0
end

pro centerFitModeChanged, stash
widget_control, stash.bgroupCenter, GET_VALUE=test
if (test eq 1) then widget_control, stash.bgroupOffset, SET_VALUE=0
end

PRO setFitOptions_event, ev
; Get the 'stash' structure.
WIDGET_CONTROL, ev.TOP, GET_UVALUE=stash
WIDGET_CONTROL, ev.ID, GET_UVALUE=uval
CASE ev.id OF
	stash.inputOffsetSt:
	else: begin
		CASE uval OF
		'OFFSET': offsetFitModeChanged, stash
		'CENTER': centerFitModeChanged, stash
		'OK': doChangeOptions, stash
		'CANCEL': WIDGET_CONTROL, stash.input, /DESTROY
		else:
		ENDCASE
	endcase
endcase
END

pro setFitOptions, base, baseOffset, baseFitOffset, baseFitCenter, log
common experimentwindow, set, experiment
if (set eq 0) then begin
	tmp = DIALOG_MESSAGE("Error: you need to set the FIT files first", /ERROR)
	return
endif
; Getting the number of peaks (we may get an error)
CATCH, Error_status
IF Error_status NE 0 THEN BEGIN 
	themessage = !ERROR_STATE.MSG 
	tmp = DIALOG_MESSAGE(themessage, /ERROR)
	return
endif
fitoffset = experiment->getFitOffset()
offset = experiment->getOffset()
fitcenter = experiment->getFitCenter()
; Prepare a gui
input = WIDGET_BASE(/COLUMN, Title='Fit options', /MODAL, GROUP_LEADER=base)
inputMacLa = WIDGET_LABEL(input, VALUE='Fit options', /ALIGN_CENTER, font=titlefont)
options = WIDGET_BASE(input, COLUMN=3, /GRID_LAYOUT, FRAME=1)
compLa = WIDGET_LABEL(options, VALUE='Azimuth of compression direction', /ALIGN_LEFT)
fitOffsetLa = WIDGET_LABEL(options, VALUE='Fit offset for compression', /ALIGN_LEFT)
fitCenterLa = WIDGET_LABEL(options, VALUE='Fit beam center', /ALIGN_LEFT)
inputOffsetSt = WIDGET_TEXT(options, VALUE=strtrim(string(offset),2), XSIZE=20, /EDITABLE)
values = ['No', 'Yes']
bgroupOffset = CW_BGROUP(options, values, /ROW, /EXCLUSIVE, SET_VALUE=fitoffset, UVALUE='OFFSET')
bgroupCenter = CW_BGROUP(options, values, /ROW, /EXCLUSIVE, SET_VALUE=fitcenter, UVALUE='CENTER')
compLa2 = WIDGET_LABEL(options, VALUE='(in degrees)', /ALIGN_LEFT)
compLa2 = WIDGET_LABEL(options, VALUE='(not compatible with beam center)', /ALIGN_LEFT)
compLa2 = WIDGET_LABEL(options, VALUE='(not compatible with offset)', /ALIGN_LEFT)
; Buttons
buttons = WIDGET_BASE(input,/ROW, /ALIGN_CENTER, /GRID_LAYOUT)
ok = WIDGET_BUTTON(buttons, VALUE='Ok', UVALUE='OK')
cancel = WIDGET_BUTTON(buttons, VALUE='Cancel', UVALUE='CANCEL')
; Finishing up
stash = {base: base, input: input, baseOffset: baseOffset, baseFitOffset: baseFitOffset, baseFitCenter: baseFitCenter,  log: log, inputOffsetSt:  inputOffsetSt, bgroupOffset: bgroupOffset, bgroupCenter: bgroupCenter}
WIDGET_CONTROL, input, SET_UVALUE=stash
WIDGET_CONTROL, input, /REALIZE
XMANAGER, 'setFitOptions', input
end


; ************************************************************ setHKLPlanes ****************

PRO doSetHKLPlanes, input, table, peaksSt, log
common experimentwindow, set, experiment
WIDGET_CONTROL, table, GET_VALUE=hklInfo 
experiment->setHKLInfo, hklInfo
WIDGET_CONTROL, input, /DESTROY
WIDGET_CONTROL, peaksSt, SET_VALUE=experiment->infoHKLLine()
logit, log, experiment->infoHKLTxt()
END

PRO setHKLPlanes_event, ev
; Get the 'stash' structure.
WIDGET_CONTROL, ev.TOP, GET_UVALUE=stash
WIDGET_CONTROL, ev.ID, GET_UVALUE=uval
CASE ev.id OF
	stash.input:
	stash.table:
	else: begin
		CASE uval OF
		'OK': doSetHKLPlanes, stash.input, stash.table, stash.peaksSt, stash.log
		'CANCEL': WIDGET_CONTROL, stash.input, /DESTROY
		else:
		ENDCASE
	endcase
endcase

END

PRO setHKLPlanes, base, peaksSt, log
common experimentwindow, set, experiment
if (set eq 0) then begin
	tmp = DIALOG_MESSAGE("Error: you need to set the FIT files first", /ERROR)
	return
endif
; Getting the number of peaks (we may get an error)
CATCH, Error_status
IF Error_status NE 0 THEN BEGIN 
	themessage = !ERROR_STATE.MSG 
	tmp = DIALOG_MESSAGE(themessage, /ERROR)
	return
endif
nhkl = experiment->getNHKL();
hklInfo = experiment->getHKLInfo();
; Prepare a gui to edit peak information
input = WIDGET_BASE(/COLUMN, Title='Peak information', /MODAL, GROUP_LEADER=base)
label = ['Use', 'h', 'k', 'l']   
rows = nhkl  
cols = 4  
rowlabels = strarr(nhkl)
for i=0, nhkl-1 do rowlabels[i] = "Peak " + STRTRIM(STRING(i + 1,/PRINT),2)
backgroundColors = MAKE_ARRAY( 3, cols, 2, /BYTE )  
backgroundColors[*,*,*] = 255   ; white  
table = WIDGET_TABLE(input, VALUE=hklInfo, /EDITABLE, COLUMN_LABELS=label, ROW_LABELS=rowlabels)
; BACKGROUND_COLOR=backgroundColors, )
; Buttons
buttons = WIDGET_BASE(input,/ROW, /ALIGN_CENTER, /GRID_LAYOUT)
ok = WIDGET_BUTTON(buttons, VALUE='Ok', UVALUE='OK')
cancel = WIDGET_BUTTON(buttons, VALUE='Cancel', UVALUE='CANCEL')
; Finishing up
stash = {base: base, input: input, table: table, peaksSt: peaksSt, log: log}
WIDGET_CONTROL, input, SET_UVALUE=stash
WIDGET_CONTROL, input, /REALIZE
XMANAGER, 'setHKLPLanes', input
end

; ************************************************************* setFITFiles ****************

PRO doSetFileFiles, input, dirSt, baseSt, firstSt, lastSt, digitsSt, guiBaseSt, guiFirstSt, guiLastSt, log
common experimentwindow, set, experiment
WIDGET_CONTROL, dirSt, GET_VALUE=dir
WIDGET_CONTROL, baseSt, GET_VALUE=baseFile
WIDGET_CONTROL, firstSt, GET_VALUE=firstS
WIDGET_CONTROL, lastSt, GET_VALUE=lastS
WIDGET_CONTROL, digitsSt, GET_VALUE=digitsS
first =  FIX(FLOAT(firstS[0]))
last = FIX(FLOAT(lastS[0]))
digits = FIX(FLOAT(digitsS[0]))
inputText = strarr(last-first+1)
n = 0
for j=first,last do begin
	fileindex = intformat(j,digits);
	filenameshort = strtrim(baseFile) + "_" + fileindex + ".fit"
	filename = dir + filenameshort
	if (FILE_TEST(filename) ne 1) then begin
		tmp = DIALOG_MESSAGE(filenameshort + " can not be found in " + dir + ". Keep going?", /QUESTION)
		if (tmp eq 'No') then return
	endif
	inputText(n) = filenameshort
	n = n + 1
endfor
tmp = experiment->setFITFiles(dir, baseFile, first, last, digits)
logit, log, experiment->infoFITTxt()
set = 1
WIDGET_CONTROL, guiBaseSt, SET_VALUE=baseFile
WIDGET_CONTROL, guiFirstSt, SET_VALUE=firstS
WIDGET_CONTROL, guiLastSt, SET_VALUE=lastS
WIDGET_CONTROL, input, /DESTROY
END


PRO setFitFiles_event, ev
; Get the 'stash' structure.
WIDGET_CONTROL, ev.TOP, GET_UVALUE=stash
WIDGET_CONTROL, ev.ID, GET_UVALUE=uval
CASE ev.id OF
	stash.input:
	else: begin
		CASE uval OF
		'INPUTDIR': changeDir, stash.input, stash.dirSt
		'OK': doSetFileFiles, stash.input, stash.dirSt, stash.baseSt, stash.firstSt, stash.lastSt, stash.digitsSt, stash.guiBaseSt, stash.guiFirstSt, stash.guiLastSt, stash.log
		'CANCEL': WIDGET_CONTROL, stash.input, /DESTROY
		else:
		ENDCASE
	endcase
endcase
END

PRO setFitFiles, base, guiBaseSt, guiFirstSt, guiLastSt, log
common experimentwindow, set, experiment
common fonts, titlefont, boldfont, mainfont
input = WIDGET_BASE(/COLUMN, Title='Fit files', /MODAL, GROUP_LEADER=base)
inputMacLa = WIDGET_LABEL(input, VALUE='Set location of FIT files', /ALIGN_CENTER, font=titlefont)
inputFiles = WIDGET_BASE(input, COLUMN=3, /GRID_LAYOUT, FRAME=1)
inputFilesDirLa = WIDGET_LABEL(inputFiles, VALUE='Directory with FIT files', /ALIGN_LEFT)
inputFilesBaseLa = WIDGET_LABEL(inputFiles, VALUE='Root name of FIT files', /ALIGN_LEFT)
inputFilesFirstLa = WIDGET_LABEL(inputFiles, VALUE='First number', /ALIGN_LEFT)
inputFilesLastLa = WIDGET_LABEL(inputFiles, VALUE='Last number', /ALIGN_LEFT)
inputFilesDigitsLa = WIDGET_LABEL(inputFiles, VALUE='Number of digits for file numbers', /ALIGN_LEFT)
inputFilesDirSt = WIDGET_TEXT(inputFiles, VALUE=experiment->getDirectory(), XSIZE=20)
inputFilesBaseSt = WIDGET_TEXT(inputFiles, VALUE=experiment->getBase(), XSIZE=20, /EDITABLE)
inputFilesFirstSt = WIDGET_TEXT(inputFiles, VALUE=STRTRIM(STRING(experiment->getFirst(),/PRINT),2), XSIZE=10, /EDITABLE)
inputFilesLastSt = WIDGET_TEXT(inputFiles, VALUE=STRTRIM(STRING(experiment->getLast(),/PRINT),2), XSIZE=10, /EDITABLE)
inputFilesDigitsSt = WIDGET_TEXT(inputFiles, VALUE=STRTRIM(STRING(experiment->getDigits(),/PRINT),2), XSIZE=10, /EDITABLE)
inputFilesDirChg = WIDGET_BUTTON(inputFiles, VALUE='Change', UVALUE='INPUTDIR')
dummy = WIDGET_LABEL(inputFiles, VALUE='', /ALIGN_LEFT)
dummy = WIDGET_LABEL(inputFiles, VALUE='', /ALIGN_LEFT)
dummy = WIDGET_LABEL(inputFiles, VALUE='', /ALIGN_LEFT)
; Buttons
buttons = WIDGET_BASE(input,/ROW, /ALIGN_CENTER, /GRID_LAYOUT)
ok = WIDGET_BUTTON(buttons, VALUE='Ok', UVALUE='OK')
cancel = WIDGET_BUTTON(buttons, VALUE='Cancel', UVALUE='CANCEL')
; Finishing up
stash = {input: input, dirSt: inputFilesDirSt, baseSt: inputFilesBaseSt,  firstSt: inputFilesFirstSt, lastSt: inputFilesLastSt, digitsSt:inputFilesDigitsSt,  guiBaseSt: guiBaseSt, guiFirstSt: guiFirstSt, guiLastSt: guiLastSt, log:log}
WIDGET_CONTROL, input, SET_UVALUE=stash
WIDGET_CONTROL, input, /REALIZE
XMANAGER, 'setFitFiles', input
END

; ************************************************************* SaveExpToAscii *************

PRO saveExpToAscii, base, log
common default, defaultdir
common experimentwindow, set, experiment
result=dialog_pickfile(title='Save experiment as', path=defaultdir, DIALOG_PARENT=base, DEFAULT_EXTENSION='.exp', FILTER=['*.exp'], /WRITE)
if (result ne '') then begin
	if (FILE_TEST(result) eq 1) then begin
		tmp = DIALOG_MESSAGE("File exists. Overwrite?", /QUESTION)
		if (tmp eq 'No') then return
	endif
	logit, log, "Saving current experiment in " + result
	FDECOMP, result, disk, dir, name, qual, version
	defaultdir = disk+dir
	setDefaultWorkDir, defaultdir
	openw, lun, result, /get_lun
	a = experiment->saveToAscii(lun)
	free_lun, lun
	if (a ne 1) then begin
		tmp = DIALOG_MESSAGE("Error: " + a, /ERROR)
		logit, log, "\tFailed!\n"
		return
	endif
	logit, log, "\tSuccessful!\n"
endif
END

; ************************************************************* doReadExpFromAscii ********

pro doReadExpFromAscii, base, baseSt, firstSt, lastSt, peaksSt, matSt, waveSt, offsetSt, fitOffsetSt, fitCenterSt, log
common experimentwindow, set, experiment
if (set eq 1) then begin
	tmp = DIALOG_MESSAGE("Anything done with the current experiment will be lost. Proceed?", /QUESTION)
	if (tmp eq 'No') then return
endif
test = readExpFromAscii(base,log)
if (test->getTmp() ne 0) then begin
	experiment = test
	WIDGET_CONTROL, baseSt, SET_VALUE=experiment->getBase()
	WIDGET_CONTROL, firstSt, SET_VALUE=STRTRIM(STRING(experiment->getFirst(),/PRINT),2)
	WIDGET_CONTROL, lastSt, SET_VALUE=STRTRIM(STRING(experiment->getLast(),/PRINT),2)
	WIDGET_CONTROL, peaksSt, SET_VALUE=experiment->infoHKLLine()
	WIDGET_CONTROL, matSt, SET_VALUE=experiment->infoMaterialLine()
	WIDGET_CONTROL, waveSt, SET_VALUE=strtrim(string(experiment->getWavelength()),2)
	WIDGET_CONTROL, offsetSt, SET_VALUE=strtrim(string(experiment->getOffset()),2)
	if (experiment->getFitOffset() eq 1) then begin
		WIDGET_CONTROL, fitOffsetSt, SET_VALUE='Yes'
	endif else WIDGET_CONTROL, fitOffsetSt, SET_VALUE='No'
	if (experiment->getFitCenter() eq 1) then begin
		WIDGET_CONTROL, fitCenterSt, SET_VALUE='Yes'
	endif else WIDGET_CONTROL, fitCenterSt, SET_VALUE='No'
	logit, log, experiment->infoTxt()
	set = 1
endif
end

; ************************************************************* New experiment ************

PRO newExperiment, base, baseSt, firstSt, lastSt, matSt, peaksSt, log
common experimentwindow, set, experiment
if (set eq 1) then begin
	tmp = DIALOG_MESSAGE("Anything done with the current experiment will be lost. Proceed?", /QUESTION)
	if (tmp eq 'No') then return
endif
experiment = OBJ_NEW('experimentObject', '', '', 0, 0, 0)
logit, log, "New experiment. Resetting everything."
setFITFiles, base, baseSt, firstSt, lastSt, log
WIDGET_CONTROL, matSt, SET_VALUE='Not set'
WIDGET_CONTROL, peaksSt, SET_VALUE='Not set'
set = 1
end

; ************************************************************* EDITMAT ********************

pro editMat, base, log, matSt
common experimentwindow, set, experiment
if (set eq 0) then begin
	tmp = DIALOG_MESSAGE("Error: you need to set the FIT files first", /ERROR)
	return
endif
newmat = materialWindow(experiment->getMaterial(), base)
if (newmat->getTmp() eq 1) then begin
	experiment->setMaterial, newmat
	WIDGET_CONTROL, matSt, SET_VALUE=experiment->infoMaterialLine()
	logit,log, experiment->infoMaterialTxt()
end
end 


; ****************************************** CHGWAVELENGTH *************************************

PRO chgWavelength_event, ev
common experimentwindow, set, experiment
WIDGET_CONTROL, ev.TOP, GET_UVALUE=stash
WIDGET_CONTROL, ev.ID, GET_UVALUE=uval
if (uval eq 'OK') then begin
    WIDGET_CONTROL, stash.waveText, GET_VALUE=wavelength
    wavlength = float(wavelength)
	experiment->setWavelength, wavelength
    WIDGET_CONTROL, stash.widget, SET_VALUE=STRTRIM(STRING(wavelength, /PRINT),2)
    logit, stash.log, experiment->infoWaveTxt()
endif
WIDGET_CONTROL, ev.TOP, /DESTROY
END

PRO chgWavelength, base, widget, log
common experimentwindow, set, experiment
basedialog = WIDGET_BASE(/COLUMN, /MODAL, GROUP_LEADER=base)
waveBase =  WIDGET_BASE(basedialog,/ROW)
waveLa = WIDGET_LABEL(waveBase, VALUE='Wavelength (in angstroms)', /ALIGN_LEFT)
waveText = WIDGET_TEXT(waveBase, XSIZE=10, VALUE=STRING(experiment->getWavelength(),/PRINT), /EDITABLE)
waveButtons = WIDGET_BASE(basedialog,/ROW, /GRID_LAYOUT, /ALIGN_CENTER)
ok = WIDGET_BUTTON(waveButtons, VALUE='Ok', UVALUE='OK')
cancel = WIDGET_BUTTON(waveButtons, VALUE='Cancel', UVALUE='CANCEL')
stash = {widget:widget, waveText:waveText, log:log}
WIDGET_CONTROL, basedialog, SET_UVALUE=stash
WIDGET_CONTROL, basedialog, /REALIZE
XMANAGER, 'chgWavelength', basedialog
END

; ************************************************************* Main window ****************

PRO experimentWindow_event, ev
; Get the 'stash' structure.
WIDGET_CONTROL, ev.TOP, GET_UVALUE=stash
WIDGET_CONTROL, ev.ID, GET_UVALUE=uval
CASE ev.id OF
	stash.base:
	stash.log:
	else: begin
		CASE uval OF
		'NEW': newExperiment, stash.base, stash.baseSt, stash.firstSt, stash.lastSt, stash.matSt, stash.peaksSt, stash.log
		'SAVE': saveExpToAscii, stash.base, stash.log
		'READ': doReadExpFromAscii, stash.base, stash.baseSt, stash.firstSt, stash.lastSt, stash.peaksSt, stash.matSt, stash.waveSt, stash.offsetSt, stash.fitOffsetSt, stash.fitCenterSt, stash.log
		'EXIT': reallyquit, stash.base
		'FITFILES': setFITFiles, stash.base, stash.baseSt, stash.firstSt, stash.lastSt, stash.log
		'MATERIAL':  editMat, stash.base, stash.log, stash.matSt
		'HKLPLANES': setHKLPlanes, stash.base, stash.peaksSt, stash.log
		'FITOPTIONS': setFitOptions, stash.base, stash.offsetSt, stash.fitOffsetSt,  stash.fitCenterSt, stash.log
		'TESTLATTICESTRAINS': testLatticeStrainsWindow, stash.base
		'FITBEAMCENTER': fitBeamCenterWindow, stash.base
		'FITOFFSET': fitOffsetWindow, stash.base
		'FITUNITCELL': fitUnitCellWindow, stash.base
		'FITPRESSURE': fitPressureWindow, stash.base
		'FITLATTICESTRAINS': fitLatticeStrainsWindow, stash.base
		'FITSTRESS': fitStressWindow, stash.base
		'WAVE': chgWaveLength, stash.base, stash.waveSt, stash.log
		'DIFFRINTENSITIES': diffIntensityWindow, stash.base
		'INTVSIMAGE': diffIntensityWindow2, stash.base
		'ABOUT': aboutWindow, stash.base
		'NOTAVAILABLE': tmp = DIALOG_MESSAGE("This function is not implemented yet!", /ERROR)
		'FORBIDDEN': tmp = DIALOG_MESSAGE("You need to registered", /ERROR)
		else:
		ENDCASE
	endcase
endcase
END

PRO experimentWindow
common experimentwindow, set, experiment
common fonts, titlefont, boldfont, mainfont
set = 0
load_defaults
experiment = OBJ_NEW('experimentObject', '', '', 0, 0, 0)
; base GUI
base = WIDGET_BASE(Title='Stess and strain analysis',/COLUMN, MBAR=bar, /TLB_SIZE_EVENTS)
; File menu
file_menu = WIDGET_BUTTON(bar, VALUE='File', /MENU) 
file_bttn1 = WIDGET_BUTTON(file_menu, VALUE='New experiment', UVALUE='NEW')
file_bttn1 = WIDGET_BUTTON(file_menu, VALUE='Open experiment', UVALUE='READ')
file_bttn2 = WIDGET_BUTTON(file_menu, VALUE='Save experiment', UVALUE='SAVE')
file_bttn3 = WIDGET_BUTTON(file_menu, VALUE='Exit', UVALUE='EXIT', /SEPARATOR)
; Experiment menu
exp_menu = WIDGET_BUTTON(bar, VALUE='Experiment', /MENU) 
exp_bttn1 = WIDGET_BUTTON(exp_menu, VALUE='Set FIT files', UVALUE='FITFILES')
exp_bttn2 = WIDGET_BUTTON(exp_menu, VALUE='Set material properties', UVALUE='MATERIAL')
exp_bttn3 = WIDGET_BUTTON(exp_menu, VALUE='Set lattice planes', UVALUE='HKLPLANES')
exp_bttn4 = WIDGET_BUTTON(exp_menu, VALUE='Set wavelength', UVALUE='WAVE')
exp_bttn5 = WIDGET_BUTTON(exp_menu, VALUE='Fit options', UVALUE='FITOPTIONS')
; Fit menu
fit_menu = WIDGET_BUTTON(bar, VALUE='Stress and strain', /MENU)
fit_bttn1 = WIDGET_BUTTON(fit_menu, VALUE='Test fit quality', UVALUE='TESTLATTICESTRAINS')
fit_bttn1 = WIDGET_BUTTON(fit_menu, VALUE='Beam centers', UVALUE='FITBEAMCENTER')
fit_bttn1 = WIDGET_BUTTON(fit_menu, VALUE='Maximum stress direction', UVALUE='FITOFFSET')
fit_bttn1 = WIDGET_BUTTON(fit_menu, VALUE='Unit cells', UVALUE='FITUNITCELL')
fit_bttn2 = WIDGET_BUTTON(fit_menu, VALUE='Pressures', UVALUE='FITPRESSURE')
fit_bttn3 = WIDGET_BUTTON(fit_menu, VALUE='Lattice strains', UVALUE='FITLATTICESTRAINS')
fit_bttn4 = WIDGET_BUTTON(fit_menu, VALUE='Stresses', UVALUE='FITSTRESS')
; Texture menu
texture_menu = WIDGET_BUTTON(bar, VALUE='Texture', /MENU)
texture_bttn1 = WIDGET_BUTTON(texture_menu, VALUE='Diffraction intensities', UVALUE='DIFFRINTENSITIES')
texture_bttn2 = WIDGET_BUTTON(texture_menu, VALUE='Intensity vs image', UVALUE='INTVSIMAGE')
; About menu 
about_menu = WIDGET_BUTTON(bar, VALUE='About...', /MENU, /ALIGN_RIGHT) 
about_bttn1 = WIDGET_BUTTON(about_menu, VALUE='About this program', UVALUE='ABOUT')
; top container
top = WIDGET_BASE(base,/ROW)
summary =  WIDGET_BASE(top,/COLUMN, FRAME=1)
baseLa = WIDGET_LABEL(summary, VALUE='Basename', /ALIGN_LEFT)
baseSt = WIDGET_TEXT(summary,  VALUE='Not set', XSIZE=10)
firstLa = WIDGET_LABEL(summary, VALUE='First index', /ALIGN_LEFT)
firstSt = WIDGET_TEXT(summary,  VALUE='Not set', XSIZE=10)
lastLa = WIDGET_LABEL(summary, VALUE='Last index', /ALIGN_LEFT)
lastSt = WIDGET_TEXT(summary,  VALUE='Not set', XSIZE=10)
matLa = WIDGET_LABEL(summary, VALUE='Material', /ALIGN_LEFT)
matSt = WIDGET_TEXT(summary,  VALUE='Not set', XSIZE=10)
peaksLa = WIDGET_LABEL(summary, VALUE='Peaks', /ALIGN_LEFT)
peaksSt = WIDGET_TEXT(summary,  VALUE='Not set', XSIZE=10)
waveLa = WIDGET_LABEL(summary, VALUE='Wavelength', /ALIGN_LEFT)
waveSt = WIDGET_TEXT(summary,  VALUE='Not set', XSIZE=10)
compLa = WIDGET_LABEL(summary, VALUE='Compression direction', /ALIGN_LEFT)
offsetSt = WIDGET_TEXT(summary,  VALUE='Not set', XSIZE=10)
fitOffsetLa = WIDGET_LABEL(summary, VALUE='Fit compression direction', /ALIGN_LEFT)
fitOffsetSt = WIDGET_TEXT(summary,  VALUE='Not set', XSIZE=10)
fitCenterLa = WIDGET_LABEL(summary, VALUE='Fit beam center', /ALIGN_LEFT)
fitCenterSt = WIDGET_TEXT(summary,  VALUE='Not set', XSIZE=10)
log = WIDGET_TEXT(top, XSIZE=75, YSIZE=40, /ALIGN_CENTER, /EDITABLE, /WRAP, /SCROLL)
stash = {base: base, log:log, firstSt: firstSt, lastSt: lastSt, baseSt:baseSt, matSt: matSt, peaksSt: peaksSt, waveSt: waveSt, offsetSt: offsetSt, fitOffsetSt: fitOffsetSt,  fitCenterSt: fitCenterSt}
WIDGET_CONTROL, base, SET_UVALUE=stash
WIDGET_CONTROL, base, /REALIZE
XMANAGER, 'experimentWindow', base
END
