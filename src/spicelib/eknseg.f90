!$Procedure EKNSEG ( EK, number of segments in file )

      INTEGER FUNCTION EKNSEG ( HANDLE )

! $ Abstract
!
!     Return the number of segments in a specified EK.
!
! $ Required_Reading
!
!     EK
!
! $ Keywords
!
!     EK
!     UTILITY
!
! $ Declarations

      IMPLICIT NONE

      INCLUDE 'ekfilpar.inc'

      INTEGER               HANDLE

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     HANDLE     I   EK file handle.
!
!     The function returns the number of segments in the specified
!     E-kernel.
!
! $ Detailed_Input
!
!     HANDLE   is the handle of an EK file opened for read access.
!
! $ Detailed_Output
!
!     The function returns the number of segments in the EK identified
!     by HANDLE.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If HANDLE is invalid, an error is signaled by a routine in the
!         call tree of this routine. EKNSEG will return the value zero.
!
!     2)  If an I/O error occurs while trying to read the EK, the error
!         is signaled by a routine in the call tree of this routine.
!         EKNSEG will return the value zero.
!
! $ Files
!
!     See the description of HANDLE in $Detailed_Input.
!
! $ Particulars
!
!     This routine is used to support the function of summarizing an
!     EK file. Given the number of segments in the file, a program
!     can use EKSSUM in a loop to summarize each of them.
!
! $ Examples
!
!     The numerical results shown for this example may differ across
!     platforms. The results depend on the SPICE kernels used as
!     input, the compiler and supporting libraries, and the machine
!     specific arithmetic implementation.
!
!     1) Find the number of segments on an EK.
!
!        Use the EK kernel below as test input file for loading the
!        experiment database. This kernel contains the Deep
!        Impact spacecraft sequence data based on the integrated
!        Predicted Events File covering the whole primary mission.
!
!           dif_seq_050112_050729.bes
!
!
!        Example code begins here.
!
!
!              PROGRAM EKNSEG_EX1
!              IMPLICIT NONE
!
!        C
!        C     SPICELIB functions
!        C
!              INTEGER               EKNSEG
!
!        C
!        C     Local variables.
!        C
!              INTEGER               HANDLE
!              INTEGER               NSEG
!
!        C
!        C     Open the EK file, returning the file handle
!        C     associated with the open file to the variable named
!        C     HANDLE.
!        C
!              CALL EKOPR ( 'dif_seq_050112_050729.bes', HANDLE )
!
!
!        C
!        C     Return the number of segments in the EK.
!        C
!              NSEG = EKNSEG( HANDLE )
!              WRITE(*,*) 'Number of segments = ', NSEG
!
!        C
!        C     Close the file.
!        C
!              CALL EKCLS ( HANDLE )
!
!              END
!
!
!        When this program was executed on a Mac/Intel/gfortran/64-bit
!        platform, the output was:
!
!
!         Number of segments =            2
!
!
! $ Restrictions
!
!     None.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!
! $ Version
!
!-    SPICELIB Version 1.1.0, 25-MAY-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard.
!        Added complete code example.
!
!-    SPICELIB Version 1.0.0, 26-SEP-1995 (NJB)
!
!-&


! $ Index_Entries
!
!     return number of segments in an E-kernel
!
!-&


!
!     SPICELIB functions
!
      LOGICAL               FAILED
      LOGICAL               RETURN

!
!     Non-SPICELIB functions
!
      INTEGER               ZZEKTRBS
      INTEGER               ZZEKTRSZ

!
!     Local variables
!
      INTEGER               BASE
      INTEGER               TREE

!
!     Set a default value for EKNSEG.
!
      EKNSEG  =  0

!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'EKNSEG' )
      END IF

!
!     Make sure this is a paged DAS EK.
!
      CALL ZZEKPGCH ( HANDLE, 'READ' )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'EKNSEG' )
         RETURN
      END IF

!
!     Obtain the base address of the first integer page.
!
      BASE = ZZEKTRBS ( 1 )

!
!     Look up the head node of the segment tree.
!
      CALL DASRDI ( HANDLE, BASE+SGTIDX, BASE+SGTIDX, TREE )

!
!     Get the entry count for the segment tree.
!
      EKNSEG  =  ZZEKTRSZ ( HANDLE, TREE )

      CALL CHKOUT ( 'EKNSEG' )
      RETURN
      END
