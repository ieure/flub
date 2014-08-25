!	9010A Language Compiler demo program
!
!	Note: for illustrative purposes, the extended features of the
!	9010A Language appear in lower case in this example

PROGRAM demo

	DPY-THIS IS AN EXAMPLE
	ex delay
	DPY-OF A 9010A PROGRAM
	ex delay
	DPY-PRODUCED BY THE COMPILER#


PROGRAM delay

DECLARATIONS
	assign REG1 to cntr		! counter register

	cntr = 40			! initialize counter
dloop:	DEC cntr			! count down to zero
	IF cntr > 0 GOTO dloop
