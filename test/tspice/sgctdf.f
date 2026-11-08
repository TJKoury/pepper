C$Procedure SGCTDF ( Generic Segments Create Test DAF )
 
      SUBROUTINE SGCTDF ( FILE, ND, NI, VALUE, CASES, NCASE, HANDLE )
 
C$ Abstract
C
C    Create a simple generic segments DAF for testing purposes.
C
C$ Disclaimer
C
C     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE
C     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S.
C     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE
C     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE
C     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS"
C     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY
C     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A
C     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC
C     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE
C     SOFTWARE AND RELATED MATERIALS, HOWEVER USED.
C
C     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA
C     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT
C     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND,
C     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS,
C     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE
C     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY.
C
C     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF
C     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY
C     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE
C     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE.
C
C$ Required_Reading
C
C     None.
C
C$ Keywords
C
C     TEST FAMILY
C
C$ Declarations
 
      IMPLICIT NONE
 
      CHARACTER*(*)         FILE
      INTEGER               ND
      INTEGER               NI
      DOUBLE PRECISION      VALUE
      INTEGER               CASES ( * )
      INTEGER               NCASE
      INTEGER               HANDLE
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     FILE       I   name of DAF to create.
C     ND         I   number of double precision components in descr.
C     NI         I   number of integer components in descriptor.
C     VALUE      I   double precision value to fill segments with.
C     CASES      I   array that describes the segment sizes to create.
C     NCASE      I   size of CASES array.
C     HANDLE     O   integer DAF handle.
C
C$ Detailed_Input
C
C     FILE       is the name of the DAF to create for testing.
C
C     ND,        are the number of double precision and integer
C     NI         components in each segment descriptor respectively.
C
C     VALUE      is the double precision value that is to fill each
C                of the segment arrays, except the last value.
C
C     CASES      is an array of integers that prescribe the size of
C                each segment that is to be constructed.
C
C     NCASE      is the number of entries in the CASES array.
C
C$ Detailed_Output
C
C     HANDLE     is the DAF handle associated with the file that was
C                just created.
C
C$ Parameters
C
C     None.
C
C$ Files
C
C     None.
C
C$ Exceptions
C
C     Any exceptions signaled will be generated and trapped by
C     routines in the call tree of SGCTDF.
C
C$ Particulars
C
C     This routine creates a DAF file suitable for generic segments
C     testing.  The DAF consists of multiple segments where each segment
C     has a single value encoded in the array, except the final value.
C     The final value indicates the number of elements in the segment
C     array.
C
C$ Examples
C
C     None.
C
C$ Restrictions
C
C     None.
C
C$ Author_and_Institution
C
C     F.S. Turner     (JPL)
C
C$ Literature_References
C
C     None.
C
C$ Version
C
C-    F_SGMETA Version 1.0.0, 16-JUN-1999 (FST)
C
C
C-&
 
C
C     SPICELIB Functions
C
      LOGICAL               RETURN
 
C
C     Local Variables
C
      DOUBLE PRECISION      DESCR ( 125 )
      INTEGER               I
      INTEGER               J
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'SGCTDF' )
      END IF
 
C
C     Initialize DESCR to 0.
C
      DO I = 1, 125
 
         DESCR ( I ) = 0.0D0
 
      END DO
 
C
C     Create the new DAF. First check to make certain no file of name
C     FILE exists. If it does remove it.
C
      CALL KILFIL ( FILE )
      CALL DAFONW ( FILE,
     .              'DAF/TST',
     .              ND,
     .              NI,
     .              'Test DAF F_SGMETA',
     .              0,
     .              HANDLE               )
 
C
C     Create each of the segments.  Note this is terribly inefficient,
C     but will work just fine for test cases.
C
      DO I = 1, NCASE
 
C
C        Start a new segment.  Note the contents of the descriptor are
C        irrelevant, since we are only trying to test the functionality
C        of SGMETA.
C
         CALL DAFBNA ( HANDLE, DESCR, 'Test Segment' )
 
C
C        Now load up the array.  This is where the serious inefficieny
C        comes in.
C
         DO J = 1, ( CASES(I) - 1 )
 
            CALL DAFADA ( VALUE, 1 )
 
         END DO
 
C
C        Place the number of elements into the last entry in the array
C        before ending it's creation.
C
         CALL DAFADA ( DBLE ( CASES(I) ), 1 )
 
         CALL DAFENA ( )
 
      END DO
 
      CALL CHKOUT( 'SGCTDF' )
      RETURN
 
      END
