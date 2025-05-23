String.prototype.strReverse = function () {
  var newstring = "";
  for (var s = 0; s < this.length; s++) {
    newstring = this.charAt(s) + newstring;
  }
  return newstring;
  //strOrig = ' texttotrim ';
  //strReversed = strOrig.revstring();
};

/* 
Uses the password lists from:
General: https://github.com/danielmiessler/SecLists/blob/master/Passwords/Common-Credentials/10-million-password-list-top-100000.txt
German: https://github.com/danielmiessler/SecLists/blob/master/Passwords/Common-Credentials/Language-Specific/German_common-password-list-top-100000.txt
*/
function chkPass(pwd) {
  var nScore = 0,
    nLength = 0,
    nAlphaUC = 0,
    nAlphaLC = 0,
    nNumber = 0,
    nSymbol = 0,
    nMidChar = 0,
    nRequirements = 0,
    nAlphasOnly = 0,
    nNumbersOnly = 0,
    nUnqChar = 0,
    nRepChar = 0,
    nRepInc = 0,
    nConsecAlphaUC = 0,
    nConsecAlphaLC = 0,
    nConsecNumber = 0,
    nConsecSymbol = 0,
    nConsecCharType = 0,
    nSeqAlpha = 0,
    nSeqNumber = 0,
    nSeqSymbol = 0,
    nSeqChar = 0,
    nReqChar = 0,
    nMultConsecCharType = 0,
    nCommPass = 0;
    commPass = [];
  var nMultRepChar = 1,
    nMultConsecSymbol = 1;
  var nMultMidChar = 2,
    nMultRequirements = 2,
    nMultConsecAlphaUC = 2,
    nMultConsecAlphaLC = 2,
    nMultConsecNumber = 2;
  var nReqCharType = 3,
    nMultAlphaUC = 3,
    nMultAlphaLC = 3,
    nMultSeqAlpha = 3,
    nMultSeqNumber = 3,
    nMultSeqSymbol = 3;
  var nMultLength = 4,
    nMultNumber = 4;
  var nMultSymbol = 6;
  var nTmpAlphaUC = "",
    nTmpAlphaLC = "",
    nTmpNumber = "",
    nTmpSymbol = "";
  var sAlphas = "abcdefghijklmnopqrstuvwxyz";
  var sNumerics = "01234567890";
  var sSymbols = ")!@#$%^&*()";
  var sComplexity = "Too Short";
  var nMinPwdLen = 8;

  if (pwd) {
    nScore = parseInt(pwd.length * nMultLength);
    nLength = pwd.length;
    var arrPwd = pwd.replace(/\s+/g, "").split(/\s*/);
    var arrPwdLen = arrPwd.length;

    for (var a = 0; a < arrPwdLen; a++) {
      if (arrPwd[a].match(/[A-Z]/g)) {
        if (nTmpAlphaUC !== "") {
          if (nTmpAlphaUC + 1 == a) {
            nConsecAlphaUC++;
            nConsecCharType++;
          }
        }
        nTmpAlphaUC = a;
        nAlphaUC++;
      } else if (arrPwd[a].match(/[a-z]/g)) {
        if (nTmpAlphaLC !== "") {
          if (nTmpAlphaLC + 1 == a) {
            nConsecAlphaLC++;
            nConsecCharType++;
          }
        }
        nTmpAlphaLC = a;
        nAlphaLC++;
      } else if (arrPwd[a].match(/[0-9]/g)) {
        if (a > 0 && a < arrPwdLen - 1) {
          nMidChar++;
        }
        if (nTmpNumber !== "") {
          if (nTmpNumber + 1 == a) {
            nConsecNumber++;
            nConsecCharType++;
          }
        }
        nTmpNumber = a;
        nNumber++;
      } else if (arrPwd[a].match(/[^a-zA-Z0-9_]/g)) {
        if (a > 0 && a < arrPwdLen - 1) {
          nMidChar++;
        }
        if (nTmpSymbol !== "") {
          if (nTmpSymbol + 1 == a) {
            nConsecSymbol++;
            nConsecCharType++;
          }
        }
        nTmpSymbol = a;
        nSymbol++;
      }
      var bCharExists = false;
      for (var b = 0; b < arrPwdLen; b++) {
        if (arrPwd[a] == arrPwd[b] && a != b) {
          bCharExists = true;
          nRepInc += Math.abs(arrPwdLen / (b - a));
        }
      }
      if (bCharExists) {
        nRepChar++;
        nUnqChar = arrPwdLen - nRepChar;
        nRepInc = nUnqChar ? Math.ceil(nRepInc / nUnqChar) : Math.ceil(nRepInc);
      }
    }

    for (var s = 0; s < 23; s++) {
      var sFwd = sAlphas.substring(s, parseInt(s + 3));
      var sRev = sFwd.strReverse();
      if (
        pwd.toLowerCase().indexOf(sFwd) != -1 ||
        pwd.toLowerCase().indexOf(sRev) != -1
      ) {
        nSeqAlpha++;
        nSeqChar++;
      }
    }

    for (var s = 0; s < 8; s++) {
      var sFwd = sNumerics.substring(s, parseInt(s + 3));
      var sRev = sFwd.strReverse();
      if (
        pwd.toLowerCase().indexOf(sFwd) != -1 ||
        pwd.toLowerCase().indexOf(sRev) != -1
      ) {
        nSeqNumber++;
        nSeqChar++;
      }
    }

    for (var s = 0; s < 8; s++) {
      var sFwd = sSymbols.substring(s, parseInt(s + 3));
      var sRev = sFwd.strReverse();
      if (
        pwd.toLowerCase().indexOf(sFwd) != -1 ||
        pwd.toLowerCase().indexOf(sRev) != -1
      ) {
        nSeqSymbol++;
        nSeqChar++;
      }
    }

    if (nAlphaUC > 0 && nAlphaUC < nLength) {
      nScore = parseInt(nScore + (nLength - nAlphaUC) * 2);
    }
    if (nAlphaLC > 0 && nAlphaLC < nLength) {
      nScore = parseInt(nScore + (nLength - nAlphaLC) * 2);
    }
    if (nNumber > 0 && nNumber < nLength) {
      nScore = parseInt(nScore + nNumber * nMultNumber);
    }
    if (nSymbol > 0) {
      nScore = parseInt(nScore + nSymbol * nMultSymbol);
    }
    if (nMidChar > 0) {
      nScore = parseInt(nScore + nMidChar * nMultMidChar);
    }

    if ((nAlphaLC > 0 || nAlphaUC > 0) && nSymbol === 0 && nNumber === 0) {
      nScore = parseInt(nScore - nLength);
      nAlphasOnly = nLength;
    }
    if (nAlphaLC === 0 && nAlphaUC === 0 && nSymbol === 0 && nNumber > 0) {
      nScore = parseInt(nScore - nLength);
      nNumbersOnly = nLength;
    }
    if (nRepChar > 0) {
      nScore = parseInt(nScore - nRepInc);
    }
    if (nConsecAlphaUC > 0) {
      nScore = parseInt(nScore - nConsecAlphaUC * nMultConsecAlphaUC);
    }
    if (nConsecAlphaLC > 0) {
      nScore = parseInt(nScore - nConsecAlphaLC * nMultConsecAlphaLC);
    }
    if (nConsecNumber > 0) {
      nScore = parseInt(nScore - nConsecNumber * nMultConsecNumber);
    }
    if (nSeqAlpha > 0) {
      nScore = parseInt(nScore - nSeqAlpha * nMultSeqAlpha);
    }
    if (nSeqNumber > 0) {
      nScore = parseInt(nScore - nSeqNumber * nMultSeqNumber);
    }
    if (nSeqSymbol > 0) {
      nScore = parseInt(nScore - nSeqSymbol * nMultSeqSymbol);
    }

    var arrChars = [nLength, nAlphaUC, nAlphaLC, nNumber, nSymbol];
    for (var c = 0; c < arrChars.length; c++) {
      if (arrChars[c] > 0) {
        nReqChar++;
      }
    }
    nRequirements = nReqChar;
    if (pwd.length >= nMinPwdLen) {
      var nMinReqChars = 3;
    } else {
      var nMinReqChars = 4;
    }
    if (nRequirements > nMinReqChars) {
      nScore = parseInt(nScore + nRequirements * 2);
    }

    // Check for common passwords
    for (var i = 0; i < commonPasswords.length; i++) {
      if (pwd.toLowerCase().includes(commonPasswords[i].toLowerCase())) {
        commPass.push(commonPasswords[i]);
        nCommPass += Math.round(commonPasswords[i].length * 4 * (commonPasswords[i].length/pwd.length));
      }
    }
    nScore -= nCommPass;

    if (nScore > 100) {
      nScore = 100;
    } else if (nScore < 0) {
      nScore = 0;
    }
    if (nScore >= 0 && nScore < 20) {
      sComplexity = "Very Weak";
    } else if (nScore >= 20 && nScore < 40) {
      sComplexity = "Weak";
    } else if (nScore >= 40 && nScore < 60) {
      sComplexity = "Good";
    } else if (nScore >= 60 && nScore < 80) {
      sComplexity = "Strong";
    } else if (nScore >= 80 && nScore <= 100) {
      sComplexity = "Very Strong";
    }

    return {
      totalScore: nScore,
      complexity: sComplexity,
      scores: {
        additions: {
          length: nLength * nMultLength,
          upperCase: nAlphaUC > 0 && nAlphaUC < nLength ? (nLength - nAlphaUC) * 2 : 0,
          lowerCase: nAlphaLC > 0 && nAlphaLC < nLength ? (nLength - nAlphaLC) * 2 : 0,
          numbers: nNumber > 0 && nNumber < nLength ? nNumber * nMultNumber : 0,
          symbols: nSymbol * nMultSymbol,
          middleNumbersOrSymbols: nMidChar * nMultMidChar,
          requirements: nRequirements > nMinReqChars ? nRequirements * nMultRequirements : 0,
        },
        deductions: {
          lettersOnly: nAlphasOnly,
          numbersOnly: nNumbersOnly,
          repeatCharacters: nRepInc,
          consecutiveUpperCase: nConsecAlphaUC * nMultConsecAlphaUC,
          consecutiveLowerCase: nConsecAlphaLC * nMultConsecAlphaLC,
          consecutiveNumbers: nConsecNumber * nMultConsecNumber,
          sequentialLetters: nSeqAlpha * nMultSeqAlpha,
          sequentialNumbers: nSeqNumber * nMultSeqNumber,
          sequentialSymbols: nSeqSymbol * nMultSeqSymbol,
          commonPassword: nCommPass,
        },
      },
      matchedCommonPasswords: commPass,
    };
  } else {
    return {
      totalScore: 0,
      complexity: sComplexity,
      scores: {
        additions: {
          length: 0,
          upperCase: 0,
          lowerCase: 0,
          numbers: 0,
          symbols: 0,
          middleNumbersOrSymbols: 0,
          requirements: 0,
        },
        deductions: {
          lettersOnly: 0,
          numbersOnly: 0,
          repeatCharacters: 0,
          consecutiveUpperCase: 0,
          consecutiveLowerCase: 0,
          consecutiveNumbers: 0,
          sequentialLetters: 0,
          sequentialNumbers: 0,
          sequentialSymbols: 0,
          commonPassword: 0,
        },
      },
    };
  }
}
