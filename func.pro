; ****************************************** intformat *********************************

function intformat, n, ndigits
case ndigits of
	'1': s = string(n,format='(I1.1)')
	'2': s = string(n,format='(I2.2)')
	'3': s = string(n,format='(I3.3)')
	'4': s = string(n,format='(I4.4)')
	'5': s = string(n,format='(I5.5)')
	'6': s = string(n,format='(I6.6)')
	'7': s = string(n,format='(I7.7)')
	'8': s = string(n,format='(I8.8)')
	'9': s = string(n,format='(I9.9)')
	'10': s = string(n,format='(I10.10)')
endcase
return, s
end


; ****************************************** fltformat *********************************

function fltformatA, f
s = string(f,format='(F8.5)')
return, s
end

function fltformatB, f
s = string(f,format='(F8.2)')
return, s
end

function fltformatC, f
s = string(f,format='(e8.1)')
return, s
end

; ****************************************** readascii **************
; readascii: 
; reads a line out of an ascii file, but skips lines that start with a
; given comment character

function readascii, lun, com=com
IF N_Elements(com) EQ 0 THEN com = ''
test = 1
while (test) do begin
	if (EOF(lun)) then return, 'EOF'
	row = STRARR(1)
	READF, lun, row
	if (strmid(row,0,1) ne com) then return, row
endwhile
end

PRO printascii, lun, txt
row = strsplit(txt, '\\n', /extract, /REGEX, /PRESERVE_NULL)
for i=0, N_ELEMENTS(row)-1 do begin
	col = strsplit(row[i], '\\t', /extract, /REGEX, /PRESERVE_NULL )
	text = ""
	for j=0, N_ELEMENTS(col)-1 do begin
		text += col[j]
		if (j lt N_ELEMENTS(col)-1) then text += "    "
	endfor
	printf, lun, text
endfor
END



function dhklhexa, x, A
	d = A[0]/sqrt(4.*(x.h*x.h+x.k*x.k+x.h*x.k)/3.+A[0]*A[0]*x.l*x.l/(A[1]*A[1]))
	return, d
end

function dhklcubic, x, A
	d = A[0]/sqrt(x.h*x.h + x.k*x.k +x.l*x.l)
	return, d
end