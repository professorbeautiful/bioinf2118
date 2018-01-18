﻿<HTML>
<BODY><p>
<h4><b>Syllabus:</b></h4>
<table width="801" border="0">
  <tr>
    <td width="58">NOTES: </td>
    <td width="727">The class number does not always correspond to the document
    number. </td>
  </tr>
  <tr>
    <td>&nbsp;</td>
    <td>This syllabus may undergo some in-flight modifications.</td>
  </tr>
  <tr>
    <td>&nbsp;</td>
    <td>Some of the documents may receive some improvements with time. Occasionally, visit the listing below &amp; sort by date.</td>
  </tr>
</table>
<p><br>
</p>
<TABLE border="1"> 
  <tr bgcolor="#E6E6E6">
   <td><b>Session</b><br>&nbsp;</td>
   <td><b>Date</b><br>&nbsp;</td>
   <td><b>Topic</b><br>&nbsp;</td>
   <td><b>Notes and Handouts</b><br>&nbsp;</td>
   <td><b>HW Assignment</b><br>&nbsp;</td>
 </tr>
<?php

$bgcolorArray = array("\"#FEEEEE\"", "\"#E6E6E6\"");
$dayArray = array("<br>Tuesday", "<br>Thursday");
$dates = array(
"09 Jan", "11 Jan", "16 Jan", "18 Jan", "23 Jan", "25 Jan", "30 Jan",
"01 Feb", "06 Feb", "08 Feb", "13 Feb", "15 Feb", "20 Feb", "22 Feb",
"27 Feb", "01 Mar", "06 Mar", "08 Mar", "13 Mar", "15 Mar", "20 Mar",
"22 Mar", "27 Mar", "29 Mar",
"03 Apr", "05 Apr", "10 Apr", "12 Apr", "17 Apr", "19 Apr", "24 Apr",
"26 Apr");

$nClasses = count($dates);
$hw = '';
$entries = array(
array(
   'Introduction to probability, likelihoods, and notation.',
   '<a
       href="N-files-for-handouts/N01a-Introduction%20to%20Probability.docx">N
       01a</a>&nbsp		
    <a
       href="N-files-for-handouts/N01b-concepts%20list.docx">
     N 01b</a>&nbsp
   <br>
   <a href="https://pitt.hosted.panopto.com/Panopto/Pages/Viewer.aspx?id=e1e2f75b-2605-47c2-bbb9-a863016f6f5d">Panopto Recording (Jan 9)</a>',
   '<a
     href="_Assignments-2118/Assignment%201,%20Bioinf%202118.docx">Homework
     1</a> assigned.<br>
     Read Dalgaard Chapter 1.1.1-1.2.7<br>
     Read Shahbaba Chapter 1.1-1.5&nbsp;'
), array(
   '<br>Introduction to R, Rstudio, and Rmarkdown. Tables in R.
    <br>Conditional, marginal, and joint probabilities.
    <br>Conditional probability, causality, and Bayes ideas.<br><br> ',
   '
    <a href="N-files-for-handouts/N02-More Probability.docx">N 02</a>&nbsp
   ',
   '&nbsp;'
), array(
   'Random variables  and distributions : <br> Discrete random variables&nbsp;',
   '<a href="N-files-for-handouts/N03-Discrete Random Variables.docx">N 03</a>
   <br>
   <a href="https://pitt.hosted.panopto.com/Panopto/Pages/Viewer.aspx?id=1e367d62-434a-4ae1-b572-af56ef0cad35">
   	Panopto Recording (Jan 16)</a>
   <br>
    <a href="N-files-for-handouts/pascalsTriangle.docx">pascalsTriangle.docx</a>',
   'Homework 1 due.&nbsp;   <br>
   <a
     href="_Assignments-2118/Assignment%202,%20Bioinf%202118.docx">
     Homework 2</a> assigned.<br>&nbsp;
   '
)

 , array(
    '
  Random variables and densities : <br> Continuous random variables<br>
   ','
   <a href="R/simulateDiagnosticData.R"> simulateDiagnosticData.R </a>
    <br>
    <a href="R/simulateDiagnosticData.Rmd"> simulateDiagnosticData.Rmd </a>
    <br> 
   <a href="N-files-for-handouts/N04-Continuous Random Variables.docx">N 04</a>&nbsp;
    '
), array(
   'Expectation and variance<br>
   <a
     href="https://pitt.hosted.panopto.com/Panopto/Pages/Viewer.aspx?id=6161697f-3013-496c-848c-c873490b2914">
      Panopto Recording (2017)</a>
    ','
    <a href="N-files-for-handouts/N05-Expectation and Variance.docx">N 05</a>&nbsp
','
    Homework 2 due.<br> 
    <a
     href="_Assignments-2118/Assignment%203,%20Bioinf%202118.docx">
     Homework 3</a> assigned.
     <br>
     Read Shahbaba, Chapter 5 through
     5.4.3. Write  at least one Q and one Aha.'
), array(
   'Question and Aha Based Review<br> Expectation and variance<br>
   <a href="https://pitt.hosted.panopto.com/Panopto/Pages/Viewer.aspx?id=ea0f3c99-4eea-474e-acf5-f129fa4425d4">Panopto Recording (2017)</a><br>',
   '&nbsp;','&nbsp;'
), array(
   'Variance and regression (conditional expectation)<br> 
   <a href="https://pitt.hosted.panopto.com/Panopto/Pages/Viewer.aspx?id=5e4d261c-0e69-4422-b397-4b4b7dee7e6b">Panopto Recording (2017)</a>',
   '<a href="N-files-for-handouts/N06-Variance and Regression.docx">N 06 (R code included)</a>&nbsp;
   <br>',
   '<a
   href="_Assignments-2118/Assignment%204,%20Bioinf%202118.docx">
   Homework 4</a> assigned.<br>'
), array(
   'Question and Aha Based Review <br> Estimation and intervals<br>
    <a href="https://pitt.hosted.panopto.com/Panopto/Pages/Viewer.aspx?id=4799a71a-b062-4ff4-8025-90485b53ca54">Panopto Recording (2017)</a><br>',
   '<a href="N-files-for-handouts/N07-estimation.docx">N 07</a>&nbsp
    <a href="N-files-for-handous/N07-extra-on-the-chi-square.docx">More on Chi-square</a>&nbsp;
   ',
   'Homework 3 due.<br>'
), array(
   'Estimation  and intervals<br>',
   '<a href="N-files-for-handouts/N08-estimation-confidence%20intervals.docx">N 08</a>&nbsp;
   ',
   '&nbsp;'
), array(
   'Question and Aha Based Review<br>Estimation and intervals<br>
    <a
      href="https://pitt.hosted.panopto.com/Panopto/Pages/Viewer.aspx?id=dd1f1378-7ac6-472b-9308-6e00e2c2b5bb">
          Panopto Recording (2017)</a>
        <a href="N-files-for-handouts/N09-estimation-plug-in-principle.docx">N 09</a>&nbsp;
       <a href="N-files-for-handouts/N10-estimation-t-intervals-pivotal-quantities.docx">N 10</a>&nbsp;
   ',
   '<a href="_Assignments-2118/Assignment%205,%20Bioinf%202118.docx">Homework 5</a> assigned.<br>
   Homework 4 due.<br>'
), array(
   'Hypothesis testing<br>
   <a href="https://pitt.hosted.panopto.com/Panopto/Pages/Viewer.aspx?id=8f102bc1-3cde-42eb-ab06-05450967a8b6">Panopto Recording (2017)</a>',
   '<a href="N-files-for-handouts/N11-testing, part 1.docx">N 11</a>&nbsp;
   <a href="N-files-for-handouts/N12-testing, one and two sample.docx">N 12</a>&nbsp;
   <a href="R-code-miscellaneous/normal%20and%20t%20test%20one-sample.Rmd">R Code: One-sample normal and t-tests</a>',
   '&nbsp;'
), array(
   'Question and Aha Based Review<br>
    Hypothesis testing
    &nbsp;',
    '&nbsp;',
   'Homework 5 due.<br>'
), array(
   'Hypothesis testing<br>
    <a href="https://pitt.hosted.panopto.com/Panopto/Pages/Viewer.aspx?id=39b3b22b-7ddb-4dd4-bbb7-5e61e9133bcf ">Panopto Recording (2017)</a>',
   '&nbsp;',
   '<a href="_Assignments-2118/Assignment%206,%20Bioinf%202118.docx">Homework 6 Assigned</a>'
), array(
   'Hypothesis Testing<br>
    Testing and the likelihood function<br>
    Demonstration of likelihood ratio test:<br>
     <a href="N-files-for-handouts/_R%20code/LRT-binomial.Rmd">(R Markdown file)</a><br>
    &nbsp;',
   '<a href="N-files-for-handouts/N13-testing, part 3 - F test.docx">N 13</a>&nbsp
   <a href="N-files-for-handouts/N14-testing, part 4 - nonparametric.docx">N 14</a>&nbsp
   ',
   '&nbsp;'
)
);
for($i=0; $i<$nClasses; $i++) {
 $bgcolor = $bgcolorArray[$i & 1];  
 $day = $dayArray[($i & 1)];  
 echo " <tr bgcolor=" . $bgcolor . ">";
 echo " <td><b>" . ($i + 1) . "</b></td>";
 echo " <td><b>" . $dates[$i] . " " . $day .  "</b></td>";
 echo " <td><b>" . $entries[$i][0] . "</b></td>";
 echo " <td><b>" . $entries[$i][1]. "</b></td>";
 echo " <td><b>" . $entries[$i][2] . "</b></td>";
 echo " </tr>";

}
?>
</table>


<hr>

</BODY>
</HTML>
