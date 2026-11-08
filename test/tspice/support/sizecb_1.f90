
!$Procedure SIZECB ( Size of character buffer )

      INTEGER FUNCTION SIZECB_1 ( BUFFER )
      IMPLICIT NONE


! $ Abstract
!
!     Return the total size of a character buffer.
!
! $ Disclaimer
!
!     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE
!     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S.
!     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE
!     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE
!     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS"
!     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY
!     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A
!     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC
!     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE
!     SOFTWARE AND RELATED MATERIALS, HOWEVER USED.
!
!     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA
!     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT
!     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND,
!     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS,
!     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE
!     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY.
!
!     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF
!     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY
!     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE
!     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE.
!
! $ Required_Reading
!
!     CB
!
! $ Keywords
!
!     ASCII
!     CHARACTER
!     STRING
!     TEXT
!
! $ Declarations

      INTEGER               LBCBUF
      PARAMETER           ( LBCBUF = 0 )

      CHARACTER*(*)         BUFFER      ( LBCBUF:* )

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     BUFFER     I   Character buffer.
!
! $ Detailed_Input
!
!     BUFFER      is a character buffer.
!
! $ Detailed_Output
!
!     The function returns the total size of the character buffer
!     (as established by a previous call to CBINIT). This is the
!     total number of characters that can be stored in the buffer.
!
! $ Files
!
!     None.
!
! $ Exceptions
!
!     None.
!
! $ Particulars
!
!     The size of a character buffer is checked before any operation
!     is performed on the buffer, to detect possible overflows.
!
! $ Examples
!
!     The code fragment
!
!        INTEGER            LBCBUF
!        PARAMETER        ( LBCBUF = 0 )
!
!        INTEGER            DIMCB
!        INTEGER            SIZECB
!        CHARACTER*100      BUFFER  ( LBCBUF:200 )
!
!        CALL CBINIT ( 200, BUFFER )
!
!        WRITE (*,*) DIMCB  ( BUFFER ),    ' elements at '
!        WRITE (*,*) LEN    ( BUFFER(1) ), ' characters each totals '
!        WRITE (*,*) SIZECB ( BUFFER ),    ' characters of storage.'
!
!     produces the following output.
!
!        200    elements at
!        100    characters each totals
!        200000 characters of storage.
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
!     Dagny Taggart, (JPL)
!
! $ Version
!
!-    Beta Version 1.1.0, 28-DEC-1994 (WLT)
!
!        The function is assigned an initial value of 0 so that it
!        will have some value if we are in RETURN mode.
!
!-    Beta Version 1.0.0, 19-JAN-1989 (DT)
!
!-&


!
!     SPICELIB functions
!
      INTEGER               DIMCB_1
      LOGICAL               RETURN

!
!     Give the function some initial value.  Zero seems as good as
!     anything.
!
      SIZECB_1 = 0

!
!     Standard error handling.
!
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'SIZECB_1' )
      END IF

!
!     Size = dimension * length.
!
      SIZECB_1 = DIMCB_1 ( BUFFER ) * LEN ( BUFFER(1) )

      CALL CHKOUT ( 'SIZECB_1' )
      RETURN
      END
