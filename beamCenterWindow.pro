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
; 
; ***************************************************************************
; plot BC function
; plots beam center position as a function of image number
; **************************************************************************

pro plotBeamCenter, log, base, selected
common experimentwindow, set, experiment
common default, defaultdir
; Use Count to get the number of nonzero elements:
use = WHERE(selected, nUse)
IF (nUse eq 0) THEN return
; Setting up the legend
legends = ['X center', 'Y center']
legend = strarr(nUse)
legend = legends[use]
; preparing arrays
fileindex = experiment->getfileindex()
n = n_elements(fileindex)
BC = fltarr(nUse,n)
BC[*,*] = !VALUES.F_NAN
j = 0
progressBar = Obj_New("SHOWPROGRESS", message='Calculating beam centers, please wait...')
progressBar->Start
for i=0, n-1 do begin
	index = 0
	xx = experiment->latticeStrainBeamCenter(i)
	BC[*,i] = xx[use]
	percent = 100.*i/n
	progressBar->Update, percent
endfor
progressBar->Destroy
Obj_Destroy, progressBar
plotinteractive1D, base, fileindex, BC, title = 'Beam center vs. image number', xlabel='Image number', ylabel='Beam center', legend=legend
end

; ***************************************************************************
; refineQTxt
; verbose refinement of lattice strains parameters Q
; for each diffraction pattern:
;   -> fits d0(hkl) and Q(hkl) for all peaks
;   -> prints the results in the log window
; **************************************************************************

pro refineBeamCenterTxt, log
common experimentwindow, set, experiment
n = experiment->getNFitFiles()
logit, log, "Starting beamCenter refinements"
for i=0,n-1 do begin
	logit, log, experiment->summaryBeamCenter(i)
endfor
logit, log, "Finished..."
end


pro exportRefineBeamCenterCSV, log
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
	text = experiment->summaryBeamCenterCSVAll(progressBar)
	printascii, lun, text
	free_lun, lun
	progressBar->Destroy
	Obj_Destroy, progressBar
endif
end

pro fitBeamCenterWindow_event, ev
; Get the 'stash' structure.
WIDGET_CONTROL, ev.TOP, GET_UVALUE=stash
WIDGET_CONTROL, ev.ID, GET_UVALUE=uval
CASE ev.id OF
	stash.input:
	else: begin
		CASE uval OF
		'REFINE': refineBeamCenterTxt, stash.log
		'ASCII': exportRefineBeamCenterCSV, stash.log
		'PLOTQ': BEGIN
			WIDGET_CONTROL, stash.plotwhatBC, GET_VALUE=selected
			plotBeamCenter, stash.log, stash.base, selected
		END
		'DONE': WIDGET_CONTROL, stash.input, /DESTROY
		else:
		ENDCASE
	endcase
endcase
end


pro fitBeamCenterWindow, base
common experimentwindow, set, experiment
common fonts, titlefont, boldfont, mainfont
; base GUI
input = WIDGET_BASE(Title='Beam center refinements', /COLUMN, GROUP_LEADER=base)
inputLa = WIDGET_LABEL(input, VALUE='Beam center refinements', /ALIGN_CENTER, FONT=titlefont)
fit = WIDGET_BASE(input, /ROW, FRAME=1)
; buttons1
buttons1 = WIDGET_BASE(fit,/COLUMN, /ALIGN_CENTER)
refine = WIDGET_BUTTON(buttons1, VALUE='Show details', UVALUE='REFINE')
export = WIDGET_BUTTON(buttons1, VALUE='Export to ASCII', UVALUE='ASCII')
plotBC = WIDGET_BASE(buttons1,/COLUMN, /ALIGN_CENTER, /FRAME, XSIZE = 100)
values = ['X center', 'Y center']
plotwhatBC = CW_BGROUP(plotBC, values, /COLUMN, /NONEXCLUSIVE, LABEL_TOP='Beam Centre', UVALUE='NOTHING')
plotit = WIDGET_BUTTON(plotBC, VALUE='Plot', UVALUE='PLOTQ')
; log
log = WIDGET_TEXT(fit, XSIZE=75, YSIZE=30, /ALIGN_CENTER, /EDITABLE, /WRAP, /SCROLL)
; buttons2
buttons2 = WIDGET_BASE(input,/ROW, /ALIGN_CENTER, /GRID_LAYOUT)
close = WIDGET_BUTTON(buttons2, VALUE='Close window', UVALUE='DONE')
stash = {base: base, input: input, log: log, plotwhatBC:plotwhatBC}
WIDGET_CONTROL, input, SET_UVALUE=stash
WIDGET_CONTROL, input, /REALIZE
XMANAGER, 'fitBeamCenterWindow', input
end