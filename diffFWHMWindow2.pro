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

; ***************************************************************************
; This window is copied/adapted from diffIntensitywindow2
; It is used when plotting peak widths for specific(s) diffraction azimut(s) 
; against diffraction number, for selected peak(s)
; Added 05/06/2013 N. Hilairet
; **************************************************************************

pro plotPeakWidthsVsImage, base, globalbase, sets, peaks, savemovie
common experimentwindow, set, experiment
common default, defaultdir
usePeak = WHERE(peaks, nUsePeak)
nAngles = N_ELEMENTS(sets)
angleList = experiment->getAngleList()
if ((sets[0] eq -1) or (nUsePeak eq 0)) then begin
  result = DIALOG_MESSAGE( "Error: no image or no diffraction line selected!", /CENTER , DIALOG_PARENT=base, /ERROR) 
  return
endif
; fetching data to plot
nPattern = experiment->getnumberPatterns()
legend=strarr(nAngles*nUsePeak)
data = fltarr(nAngles*nUsePeak,nPattern)
peakIndex = intarr(nUsePeak)
fileindex = experiment->getfileindex()
for i=0, nUsePeak-1 do peakIndex[i] = experiment->usedpeakindex(usePeak[i])
progressBar = Obj_New("SHOWPROGRESS", message='Processing, please wait...')
progressBar->Start
for i=0, nAngles-1 do begin
	for j=0, nUsePeak-1 do begin
		data[i*nUsePeak+j,*] = experiment->getPeakWidthAtAngle(angleList[sets[i]],peakIndex[j])
		
		;for j=0, nUsePeak-1 do begin
		;	legend[i,j] = experiment->getFileName(sets[i]) + ' ' + experiment->getPeakName(peakIndex[j])
		;endfor
		legend[i*nUsePeak+j] = string(angleList[sets[i]])+'-'+ experiment->getPeakName(peakIndex[j])
		percent = 100.*i/nAngles
		progressBar->Update, percent
	endfor
endfor
progressBar->Destroy
Obj_Destroy, progressBar
; calling the plot window
plotinteractive1D, base, fileindex, data, title = 'peak widths vs. image number', xlabel='Image number', ylabel='Peak widths', legend=legend
end

pro exportPeakWidthsVsImageCSV, base, sets, peaks
common experimentwindow, set, experiment
common default, defaultdir
result=dialog_pickfile(title='Save results as', path=defaultdir, DIALOG_PARENT=base, DEFAULT_EXTENSION='.csv', FILTER=['*.csv'], /WRITE, get_path = newdefaultdir)
if (result ne '') then begin
	defaultdir = newdefaultdir
	if (FILE_TEST(result) eq 1) then begin
		tmp = DIALOG_MESSAGE("File exists. Overwrite?", /QUESTION)
		if (tmp eq 'No') then return
	endif
	progressBar = Obj_New("SHOWPROGRESS", message='Calculating, please wait...')
	progressBar->Start
	usePeak = WHERE(peaks, nUsePeak)
	nAngles = N_ELEMENTS(sets)
	nPattern = experiment->getnumberPatterns()
	angleList = experiment->getAngleList()
	data = fltarr(nAngles*nUsePeak,nPattern)
	peakIndex = intarr(nUsePeak)
	for i=0, nUsePeak-1 do peakIndex[i] = experiment->usedpeakindex(usePeak[i])
	legend=strarr(nAngles*nUsePeak)
		for i=0, nAngles-1 do begin
			for j=0, nUsePeak-1 do begin
		data[i*nUsePeak+j,*] = experiment->getPeakWidthAtAngle(angleList[sets[i]],peakIndex[j])
		legend[i*nUsePeak+j] = experiment->getPeakName(peakIndex[j])+'-'+ strtrim(string(angleList[sets[i]]),2)
			endfor
		endfor
	n = experiment->getNFitFiles()
	images = experiment->getFileList()
	openw, lun, result, /get_lun
	printf, lun, "# Intensities at angles and peaks chosen functions of image number"
	printf, lun, "# image number", legend
		for step=0,n-1 do begin
		printf, lun, STRING(images[step]), STRING(9B), fltformatD(data[*,step])
		endfor
	free_lun, lun
	progressBar->Destroy
	Obj_Destroy, progressBar
endif
end

; *********************************************************************** Interface ****************

pro diffFWHMWindow2_event, ev
; Get the 'stash' structure.
WIDGET_CONTROL, ev.TOP, GET_UVALUE=stash
WIDGET_CONTROL, ev.ID, GET_UVALUE=uval
sets = WIDGET_INFO(stash.listSets, /LIST_SELECT)
WIDGET_CONTROL, stash.plotwhatPeak, GET_VALUE=peaks
CASE ev.id OF
	stash.input:
	else: begin
		CASE uval OF
		'PLOT': plotPeakWidthsVsImage, stash.input, stash.base, sets, peaks
		'DONE': WIDGET_CONTROL, stash.input, /DESTROY
		'ASCII': exportPeakWidthsVsImageCSV, stash.input, sets, peaks
		else:
		ENDCASE
	endcase
endcase
end

pro diffFWHMWindow2, base
common experimentwindow, set, experiment
common fonts, titlefont, boldfont, mainfont
; base GUI
input = WIDGET_BASE(Title='Peak widths vs image numbers', /COLUMN, GROUP_LEADER=base)
inputLa = WIDGET_LABEL(input, VALUE='Peak widths vs image numbers', /ALIGN_CENTER, FONT=titlefont)
fit = WIDGET_BASE(input, /ROW, FRAME=0)
; listing datasets
alist = WIDGET_BASE(fit,/COLUMN, /ALIGN_CENTER, FRAME=1,XSIZE=200, YSIZE=400)
anglelist = experiment->getAngleList()
anglelistStr = STRING(anglelist)
listLa = WIDGET_LABEL(alist, VALUE='Datasets', /ALIGN_CENTER)
listSets = Widget_List(alist, VALUE=anglelistStr, UVALUE='NOTHING', /MULTIPLE, SCR_XSIZE=190, SCR_YSIZE=360)
; Options
right = WIDGET_BASE(fit,/COLUMN, /ALIGN_CENTER, FRAME=1, YSIZE=400)
; peak list
values = experiment->getPeakList(/used)
plotwhatPeak = CW_BGROUP(right, values, /COLUMN, /NONEXCLUSIVE, LABEL_TOP='hkl', UVALUE='NOTHING', /SCROLL, Y_SCROLL_SIZE=320)
; buttons2
buttons2 = WIDGET_BASE(input,/ROW, /ALIGN_CENTER, /GRID_LAYOUT)
plot1 = WIDGET_BUTTON(buttons2, VALUE='Plot', UVALUE='PLOT')
close = WIDGET_BUTTON(buttons2, VALUE='Close window', UVALUE='DONE')
export = WIDGET_BUTTON(buttons2, VALUE='Export to ASCII', UVALUE='ASCII')
stash = {base: base, input: input, plotwhatPeak:plotwhatPeak, listSets:listSets}
WIDGET_CONTROL, input, SET_UVALUE=stash
WIDGET_CONTROL, input, /REALIZE
XMANAGER, 'diffFWHMWindow2', input
end