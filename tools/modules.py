"""
Which SPICELIB routines go into which PEPPER module (used by
tools/modularize.py). Modules are listed in dependency order: a module's
routines may use only modules listed before it.
"""

# Constant functions of SPICELIB that are named constants in
# pepper_constants: name -> kind ("wp" real, "ip" integer).
CONSTANTS = {
    **{n: "wp" for n in "b1900 b1950 clight dpmax dpmin dpr halfpi j1900 j1950 "
                        "j2000 j2100 jyear pi rpd spd twopi tyear".split()},
    **{n: "ip" for n in "intmax intmin".split()},
}

MODULES = {
    "pepper_constants": sorted(CONSTANTS),
    "pepper_arrays": """
        alltru bschoc bschoi bsrchc bsrchd bsrchi clearc cleard cleari fillc
        filld filli isordv isrchc isrchd isrchi lstcld lstcli lstlec lstled
        lstlei lstltc lstltd lstlti maxac maxad maxai minac minad minai movec
        moved movei notru orderc orderd orderi prodad prodai reordc reordd
        reordi reordl rmdupc rmdupd rmdupi sameai shellc shelld shelli somfls
        somtru sumad sumai swapc swapd swapi
        """.split(),
    "pepper_math": """
        approx axisar azlrec brcktd brckti chbder chbint chbval cyllat cylrec
        cylsph dcbrt det drdcyl drdlat drdsph ducrss dvcrss dvdot dvhat dvnorm
        el2cgv even exact frame gcd ident insang invert latcyl latrec latsph
        mequ mequg mtxm mtxmg mtxv mtxvg mxm mxmg mxmt mxmtg mxv mxvg odd
        opsgnd opsgni pl2nvc pl2nvp pl2psv pltexp pltnrm polyds q2m qdq2av qxq
        radrec rav2xf recazl reccyl reclat recrad recsph sharpr smsgnd smsgni
        sphcyl sphlat sphrec trace traceg ucrss unorm unormg vadd vaddg vcrss
        vdist vdistg vdot vdotg vequ vequg vhat vhatg vhatip vlcom vlcom3
        vlcomg vminug vminus vnorm vnormg vpack vperp vproj vprojg vrel vrelg
        vrotv vscl vsclg vsclip vsep vsepg vsub vsubg vtmv vtmvg vupack vzero
        vzerog xf2rav xpose xposeg xpsgip zzmsxf zzrxr
        """.split(),
    "pepper_strings": """
        astrip bedec beint benum beuns cmprss cpos cposr eqstr esrchc fndnwd
        frstnb frstnp frstpc kxtrct lastnb lastpc lbuild lcase ljucrs ljust
        lparse lparsm ltrim lxqstr matchw nblen nbwid ncpos ncposr nextwd nthwd
        parsqs pcwid pos posr prefix quote replch replwd repmc rjust rtrim
        samch samsub scanrj shiftl shiftr sigdgt suffix ucase wdcnt wdindx
        zzinssub zzrepsub
        """.split(),
}
