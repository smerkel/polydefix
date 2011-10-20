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
; pressure and unit cell functions
; Deal with calculations related to
; - pressure
; - unit cell parameters
; as a function of file number
;  **************************************************************************

; ***************************************************************************
; plot pressure function
; **************************************************************************

pro plotpressure, log, base
common experimentwindow, set, experiment
common default, defaultdir
fileindex = experiment->getfileindex()
n = n_elements(fileindex)
p = fltarr(n)
p[*] = !VALUES.F_NAN
progressBar = Obj_New("SHOWPROGRESS", message='Calculating pressures, please wait...')
progressBar->Start
for i=0,n-1 do begin
	;print, i, j
	cell = experiment->refinePressure(i)
	pp = cell->getPressure()
	if ((abs(pp) eq !VALUES.F_INFINITY) or (fix(pp*1000) eq 0)) then p[i]=!VALUES.F_NAN else p[i]=pp
	OBJ_DESTROY, cell
	percent = 100.*i/n
	progressBar->Update, percent
endfor
progressBar->Destroy
Obj_Destroy, progressBar
plotinteractive1D, base, fileindex, p, title = 'Pressure vs. image number', xlabel='Image number', ylabel='Pressure'
end

; ***************************************************************************
; plot volume function
; **************************************************************************

pro plotvolume, log, base
common experimentwindow, set, experiment
common default, defaultdir
fileindex = experiment->getfileindex()
n = n_elements(fileindex)
v = fltarr(n)
v[*] = !VALUES.F_NAN
progressBar = Obj_New("SHOWPROGRESS", message='Calculating volumes, please wait...')
progressBar->Start
for i=0,n-1 do begin
	cell = experiment->refineVolume(i)
	vv = cell->getVolumeNoError()
	if ((abs(vv) eq !VALUES.F_INFINITY) or (fix(vv*1000) eq 0)) then v[i]=!VALUES.F_NAN else v[i]=vv
	OBJ_DESTROY, cell
	percent = 100.*i/n
	progressBar->Update, percent
endfor
progressBar->Destroy
Obj_Destroy, progressBar
plotinteractive1D, base, fileindex, v, title = 'Volume vs. image number', xlabel='Image number', ylabel='Volume'
end


; ***************************************************************************
; refine pressure and unit cell parameters function
; ***************************************************************************

pro startRefinePressure, log
common experimentwindow, set, experiment
n = experiment->getNFitFiles()
logit, log, "Starting unit cell refinements"
for i=0,n-1 do begin
	logit, log, experiment->getFileName(i)
	cell = experiment->refinePressure(i)
	logit, log, cell->summaryPressure()
	OBJ_DESTROY, cell
endfor
logit, log, "Finished..."
end

; ***************************************************************************
; export and unit cell parameters pressure function
; ***************************************************************************

pro exportPressure, log
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
	openw, lun, result, /get_lun
	text = experiment->refineAllPressuresCVS(progressBar)
	printascii, lun, text
	free_lun, lun
	progressBar->Destroy
	Obj_Destroy, progressBar
endif
end

; ***************************************************************************
; main interface
; ***************************************************************************

pro fitPressureWindow_event, ev
; Get the 'stash' structure.
WIDGET_CONTROL, ev.TOP, GET_UVALUE=stash
WIDGET_CONTROL, ev.ID, GET_UVALUE=uval
CASE ev.id OF
	stash.input:
	else: begin
		CASE uval OF
		'REFINE': startRefinePressure, stash.log
		'EXPORT': exportPressure, stash.log
		'PLOT': BEGIN
			WIDGET_CONTROL, stash.plotwhat, GET_VALUE=selected
			if (selected eq 1) then  plotVolume, stash.log, stash.base else plotPressure, stash.log, stash.base
		END
		'DONE': WIDGET_CONTROL, stash.input, /DESTROY
		else:
		ENDCASE
	endcase
endcase
end

pro fitPressureWindow, base
common fonts, titlefont, boldfont, mainfont
; base GUI
input = WIDGET_BASE(Title='Pressure refinements', /COLUMN, GROUP_LEADER=base)
inputLa = WIDGET_LABEL(input, VALUE='Pressure refinements', /ALIGN_CENTER, font=titlefont)
fit = WIDGET_BASE(input, /ROW, FRAME=1)
; buttons1
buttons1 = WIDGET_BASE(fit,/COLUMN, /ALIGN_TOP, FRAME=0)
refine = WIDGET_BUTTON(buttons1, VALUE='Refine pressures', UVALUE='REFINE')
export = WIDGET_BUTTON(buttons1, VALUE='Export results', UVALUE='EXPORT')
plotPV = WIDGET_BASE(buttons1,/COLUMN, /ALIGN_CENTER, /FRAME, XSIZE = 100)
values = ['Pressure', 'Volume']
plotwhat = CW_BGROUP(plotPV, values, /COLUMN, /EXCLUSIVE, LABEL_TOP='Plots', UVALUE='NOTHING', SET_VALUE=0)
plotit = WIDGET_BUTTON(plotPV, VALUE='Plot', UVALUE='PLOT')
; log
log = WIDGET_TEXT(fit, XSIZE=75, YSIZE=30, /ALIGN_CENTER, /EDITABLE, /WRAP, /SCROLL)
; buttons2
buttons2 = WIDGET_BASE(input,/ROW, /ALIGN_CENTER, /GRID_LAYOUT)
close = WIDGET_BUTTON(buttons2, VALUE='Close window', UVALUE='DONE')
stash = {input: input, log: log, base: base, plotwhat:plotwhat}
WIDGET_CONTROL, input, SET_UVALUE=stash
WIDGET_CONTROL, input, /REALIZE
XMANAGER, 'fitPressureWindow', input
end