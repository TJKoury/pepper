C$Procedure T_ZZCKCV06 (Private --- C-kernel segment coverage, type 06)
 
      SUBROUTINE T_ZZCKCV06 ( HANDLE, DESCR,  SCLKID, 
     .                        TOL,    TIMSYS, SCHEDL )
 
C$ Abstract
C
C     SPICE Private routine intended solely for the support of SPICE
C     routines.Users should not call this routine directly due
C     to the volatile nature of this routine.
C
C     This is a TSPICE test routine. This routine has both an
C     alternate calling sequence and implementation relative to
C     ZZCKCV06.
C
C     Determine the "window" of coverage of a type 06 C-kernel segment.
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
C     CK
C     DAF
C
C$ Keywords
C
C     CK
C     UTILITY
C     PRIVATE
C
C$ Declarations
 
      IMPLICIT NONE
 
      INCLUDE              'ck06.inc'
 
      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )
 
      INTEGER               HANDLE
      DOUBLE PRECISION      DESCR  ( * )
      INTEGER               SCLKID
      DOUBLE PRECISION      TOL
      CHARACTER*(*)         TIMSYS
      DOUBLE PRECISION      SCHEDL ( LBCELL:* )
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     HANDLE     I   Handle of a C-kernel open for read access
C     DESCR      I   DAF segment descriptor.
C     SCLKID     I   SCLK ID.
C     TOL        I   Tolerance in ticks.
C     TIMSYS     I   Time system used to represent coverage.
C     SCHEDL    I/O  An initialized window/schedule of interval
C
C$ Detailed_Input
C
C     HANDLE     is the handle of some DAF that is open for reading.
C
C     DESCR      is the DAF descriptor of a type 06 CK segment. DESCR
C                identifies the instrument (frame class ID) for which
C                coverage is to be found. DESCR also provides the 
C                segment time coverage bounds DC(1), DC(2).
C
C                Each interpolation interval is replaced with its
C                intersection with the segment coverage interval
C
C                   [ DC(1), DC(2) ]
C
C                before being expanded by TOL. Interpolation intervals
C                that don't intersect the segment coverage interval are
C                discarded, even if after expansion by TOL they would
C                have non-empty intersection with the segment coverage
C                interval.
C                                
C     SCLKID     is the ID code of the spacecraft clock associated with
C                the object for which the segment contains pointing.
C                This is the ID code used by the SCLK conversion
C                routines.
C
C     TOL        is a tolerance value expressed in ticks of the
C                spacecraft clock associated with the segment. After
C                truncation by the segment coverage interval, and
C                before insertion into the coverage window, each
C                non-empty truncated interpolation interval is expanded
C                by TOL:  the left endpoint of each interval is reduced
C                by TOL and the right endpoint is increased by TOL.
C                Any intervals that overlap as a result of the
C                expansion are merged.
C
C                The coverage window returned when TOL > 0 indicates
C                the coverage provided by the file to the CK readers
C                CKGPAV and CKGP when that value of TOL is passed to
C                them as an input.
C
C                
C     TIMSYS     is a string indicating the time system used in the
C                output coverage window.TIMSYS may have the values:
C
C                   'SCLK'    Elements of SCHEDL are expressed in
C                             encoded SCLK ("ticks"), where the clock
C                             is associated with the object designated
C                             by IDCODE.
C
C                   'TDB'     Elements of SCHEDL are expressed as
C                             seconds past J2000 TDB.
C
C                TIMSYS must be consistent with the system used for
C                the contents of SCHEDL on input, if any.
C
C
C     SCHEDL     is a schedule (window) of intervals, to which the
C                intervals of coverage for this segment will be added.
C
C$ Detailed_Output
C
C     SCHEDL     the input schedule updated to include the intervals
C                of coverage for this segment. The schedule has
C                been adjusted to account for the provided tolerance
C                value. Coverage lying outside the interval
C
C                   DC(1) - TOL : DC(2) + TOL
C
C                is excluded.
C
C                The elements of SCHEDL are given in the time system
C                indicated by TIMSYS.
C
C$ Parameters
C
C     Several parameters associated with the type 06 C-kernel
C     are utilized to compute the packet size of each subtype.
C     See the include file 'ck06.inc' for details.
C
C$ Exceptions
C
C     1)  The error SPICE(NOTSUPPORTED) is signaled if the subtype of
C         the CK type 06 segment is not recognized.
C
C     2)  Routines in the call tree of this routine may signal errors
C         if insufficient room in SCHEDL exists or other error
C         conditions relating to file access arise.
C
C     3)  If TOL is negative, the error SPICE(VALUEOUTOFRANGE) is
C         signaled.
C
C     4)  If TIMSYS is not recognized, the error SPICE(INVALIDOPTION)
C         is signaled.
C
C     5)  If a time conversion error occurs, the error will be 
C         diagnosed by a routine in the call tree of this routine.
C
C$ Files
C
C     This routine reads the contents of the file associated with
C     HANDLE to locate coverage intervals.
C
C$ Particulars
C
C     This is a utility routine that determines the intervals
C     of coverage for a type 06 C-kernel segment.
C
C$ Examples
C
C     See CKCOV.
C
C$ Restrictions
C
C     None.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman    (JPL)
C     B.V. Semenov    (JPL)
C     W.L. Taber      (JPL)
C     F.S. Turner     (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.0.0, 05-FEB-2014 (NJB) (BVS) (FST) (WLT)
C
C-&
 
C
C     SPICELIB Functions
C
      LOGICAL               EQSTR
      LOGICAL               FAILED
      LOGICAL               RETURN

C
C     Local parameters
C
      INTEGER               ND
      PARAMETER           ( ND     = 2 )
      
      INTEGER               NI
      PARAMETER           ( NI     = 6 )

C
C     Local Variables
C
      DOUBLE PRECISION      BEGIN
      DOUBLE PRECISION      DC     ( ND )
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      FINISH
      DOUBLE PRECISION      IVLBDS ( 2 )
      DOUBLE PRECISION      LSTEPC
      DOUBLE PRECISION      RATE

      INTEGER               I
      INTEGER               IC     ( NI )
      INTEGER               NINTVL
      INTEGER               NREC
      INTEGER               SUBTYP
      INTEGER               WINSIZ

      LOGICAL               ISTDB
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'T_ZZCKCV06' )
 
C
C     Check tolerance value.
C
      IF ( TOL .LT. 0.D0 ) THEN

         CALL SETMSG ( 'Tolerance must be non-negative; actual value '//
     .                 'was #.'                                       )
         CALL ERRDP  ( '#',  TOL                                      )
         CALL SIGERR ( 'SPICE(VALUEOUTOFRANGE)'                       )
         CALL CHKOUT ( 'T_ZZCKCV06'                                   )
         RETURN

      END IF

C
C     Set a logical flag indicating whether the time system is SCLK.
C
      ISTDB = EQSTR ( TIMSYS, 'TDB' ) 

C
C     Check time system.
C
      IF ( .NOT. ISTDB ) THEN

         IF (  .NOT.  EQSTR ( TIMSYS, 'SCLK' )  )THEN

            CALL SETMSG ( 'Time system spec TIMSYS was #; allowed ' //
     .                    'values are SCLK and TDB.'                )
            CALL ERRCH  ( '#',  TIMSYS                              )
            CALL SIGERR ( 'SPICE(INVALIDOPTION)'                    )
            CALL CHKOUT ( 'T_ZZCKCV06'                              )
            RETURN

         END IF

      END IF

C
C     Unpack the descriptor.
C
      CALL DAFUS ( DESCR, ND, NI, DC, IC )

C
C     Fetch the mini-segment count from the segment.
C
      CALL CKNM06 ( HANDLE, DESCR, NINTVL )

C
C     Each mini-segment contributes a coverage interval to the
C     total coverage of the segment. Since mini-segments can
C     contain gaps, we need to examine not only the interpolation
C     interval bounds but the final epochs of the mini-segments.
C
C     Now loop over the mini-segments and find the contribution
C     from each one.
C     
      DO I = 1,  NINTVL
C
C        Find the interval bounds for this mini-segment.
C
         CALL CKMP06 ( HANDLE, DESCR, I,      RATE,  SUBTYP, 
     .                 WINSIZ, NREC,  IVLBDS, LSTEPC        )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'T_ZZCKCV06' )
            RETURN
         END IF
 

         BEGIN  = IVLBDS(1)         
C
C        The smaller of LSTEPC and IVLBDS(2) is the
C        end of the mini-segment's coverage.
C      
         FINISH = MIN ( LSTEPC, IVLBDS(2) )
C
C        Truncate the interval using the segment bounds.
C
         BEGIN   =  MAX ( BEGIN,  DC(1) )
         FINISH  =  MIN ( FINISH, DC(2) )

C
C        Adjust the interval using the tolerance. Empty
C        intervals *do not get expanded*; this choice is
C        consistent with the type 6 reading algorithm.
C
         IF ( BEGIN .LE. FINISH ) THEN

            IF ( TOL .GT. 0.D0 ) THEN

               BEGIN   =  MAX( BEGIN  - TOL, 0.D0 )
               FINISH  =       FINISH + TOL

            END IF 

         END IF

C
C        Convert the time to TDB if necessary.
C
         IF ( ISTDB ) THEN

            CALL SCT2E ( SCLKID, BEGIN,  ET )
            BEGIN  = ET

            CALL SCT2E ( SCLKID, FINISH, ET )
            FINISH = ET

            IF ( FAILED() ) THEN
               CALL CHKOUT ( 'T_ZZCKCV06' )
               RETURN
            END IF

         END IF

C
C        Insert the interval into the window.
C
         IF ( BEGIN .LE. FINISH ) THEN

            CALL WNINSD ( BEGIN, FINISH, SCHEDL )

            IF ( FAILED() ) THEN
               CALL CHKOUT ( 'T_ZZCKCV06' )
               RETURN
            END IF

         END IF

      END DO
 
 
      CALL CHKOUT ( 'T_ZZCKCV06' )
      RETURN
      END
