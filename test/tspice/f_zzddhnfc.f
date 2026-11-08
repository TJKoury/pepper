C$Procedure F_ZZDDHNFC ( ZZDDHNFC tests )
 
      SUBROUTINE F_ZZDDHNFC ( OK )
 
C$ Abstract
C
C     Exercise the SPICELIB routine ZZDDHNFC.
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

      LOGICAL               OK
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     OK         O   logical indicating test status.
C
C$ Detailed_Input
C
C     None.
C
C$ Detailed_Output
C
C     OK         is a logical that indicates the test status to the
C                caller.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     This routine does not generate any errors. Routines in its
C     call tree may generate errors that are either intentional and
C     trapped or unintentional and need reporting.  The test family
C     utilities manage this.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine tests the SPICELIB routine ZZDDHNFC.
C
C$ Examples
C
C     None.
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
C     N.J. Bachman     (JPL)
C
C$ Version
C
C-    TSPICE Version 1.0.0, 27-MAY-2016 (NJB)
C
C-&
 
C
C     SPICELIB functions
C 

C
C     Local Parameters
C 
      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )
 

C
C     Local Variables
C
      CHARACTER*(LNSIZE)    FMTSTR
      CHARACTER*(LNSIZE)    BFFSTR 
            
      INTEGER               NATBFF

C
C     Saved values
C
      SAVE

C
C     Initial values
C

C
C     Open the test family.
C
      CALL TOPEN ( 'F_ZZDDHNFC' )


C***********************************************************************
C
C     Normal cases
C
C***********************************************************************


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Fetch the integer BFF code for the host system.' )


C
C     Look up the BFF integer code.
C
      CALL ZZDDHNFC ( NATBFF ) 
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Look up the equivalent string.
C
      CALL ZZDDHGSD ( 'BFF', NATBFF, BFFSTR )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL ZZPLATFM ( 'FILE_FORMAT', FMTSTR )
      CALL CHCKXC ( .FALSE., ' ', OK )

      
      CALL CHCKSC ( 'BFFSTR', BFFSTR, '=', FMTSTR, OK )
      

C*********************************************************************
C*
C*    Error cases
C*
C*********************************************************************

C
C     None.
C
  
C
C     Close out the test family.
C
      CALL T_SUCCESS ( OK )
      RETURN 
      END


