$(document).ready(function () {
   $("a:has(span.fa-github), p>a:not(.footnote-ref, .footnote-back, .footref, #footback)")
   .attr('target','_blank')
   .attr('rel', 'noopener noreferrer');
   
   $(".navbar-brand")
   .attr('target','_top')
   .addClass('metallic-sheen');
   
   $("img:not(.modal-content, #hover-icon)")
   .attr("data-toggle","modal")
   .attr("data-target", "#imgModal")
   .attr("aria-labelledby", "imgTooltip")
   .attr("class", "img-toggle");
   
   function setCaptionText() {
      // Get all of the img elements on the page
      var imgs = document.querySelectorAll(".img-toggle");
    
      // For each img element, check if the sibling is .caption
      /// and if it is copy it as the img's alt attribute
      for (var i = 0; i < imgs.length; i++) {
        var img = imgs[i];
        var sibling = img.nextElementSibling;
        if (sibling == null) return;
        if (sibling.matches(".caption")) {
          img.alt = sibling.textContent;
          }
      }
    }
    
    // Call the setCaptionText() function when the page loads.
    window.onload = setCaptionText;
    
    window.addEventListener(
    "keydown",
    (event) => {
      // If modal is up and Esc is pressed, hide the modal
      if (event.key === "Escape" & $("body.modal-open").length){
            $("#imgModal").click()
          }
        }
      );
    
    // for a specific image that doesn't have a caption
    $("#qc_workflow").attr("alt", "The QC Workflow")
  
    var img_tooltip = document.getElementById('imgTooltip');
    
    function showTooltip () {
      img_tooltip.style.opacity = 1;
    };
    
    function hideTooltip () {
      img_tooltip.style.opacity = 0;
    };
    
    followmouse = (e) => {
      // if the mouse hovers on the tooltip itself - don't change anything
      if ($('#imgTooltip:hover').length) {
        e.preventDefault();
      // if it's over an image, show the tooltip
      } else if ($('.img-toggle:hover').length) {
        showTooltip();
        // otherwise, keep it hidden
        } else {
          hideTooltip();
        }
      // move the tooltip so when it's shown it follows the mouse
      img_tooltip.style.left = `${e.clientX + 5}px`;
      img_tooltip.style.top = `${e.clientY - 25}px`;
    }
    
    document.addEventListener('mousemove', followmouse)
    
    $("img.img-toggle").on("click", function() {
       var img_src = $(this).attr("src"),
       img_alt     = $(this).attr("alt");
       $(".modal-content").attr("src", img_src);
       $("#imgCaption").text(img_alt);
       if (img_alt == "The QC Workflow") {
               $(".modal-content")
               .css("border", "none")
               .attr("style","-webkit-box-shadow: none !important")
               .css("filter", "drop-shadow(-9px 6px 4px #00000080)");
              }
     });
     
     let original_location = $(".footref").first().position().top;
     
     $(".footref").click(function() {
       var $clicked = $(this),
       pos      = $clicked.position();
       original_location = pos.top;
       send_to = $("#footback").position().top;
       $('html').scrollTop(send_to)
     })
     
     document.getElementById("footback").addEventListener('click', function(){
        $('html').scrollTop(original_location - $(window).height()/2);
     })
});