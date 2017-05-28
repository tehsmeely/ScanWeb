$(function(){


	var images = [];
	var MAX_IMAGE_HEIGHT = 550;

	function matchHeight(){
		var maxHeight=0;
		$(".iBoxCont").each(function(){
			var h = $(this).height();
			if (h > maxHeight){
				maxHeight = h;
			}
		});
		$(".iBoxCont").height(maxHeight);
	}
	
	function setWidth(){
		$(".interactionBoxContainer").width(
			$(".scanStatusContainer").outerWidth()+
			$(".scanStartContainer").outerWidth() +
			40
		)
	}
	
	function updateBoxes(){
		matchHeight();
		setWidth();
	}
	updateBoxes();

	function setConnectionUp(){
		$("#connectionStatus").removeClass("inactive");
		$("#connectionStatus").addClass("active");
	}
	function setConnectionDown(){
		$("#connectionStatus").removeClass("active");
		$("#connectionStatus").addClass("inactive");
	}
	function setScannerAvailable(){
		$("#scanStatusText").removeClass("isBusy");
		$("#scanStatusText").addClass("isAvailable");
		$("#scanStatusText").text("Available");
		$(".activeScanInfoBox").hide();
		updateBoxes();
	}
	function setScannerBusy(StatusObj){
		$("#scanStatusText").removeClass("isAvailable");
		$("#scanStatusText").addClass("isBusy");
		$("#scanStatusText").text("Busy");

		var infoBox = $(".activeScanInfoBox");
		infoBox.empty();
		infoBox.append("<p>Started: " + StatusObj.STARTED + "</p>");
		infoBox.append("<p>Mode: " + StatusObj.MODE + "</p>");
		infoBox.append("<p>DPI: " + StatusObj.DPI + "</p>");
		infoBox.show();
		updateBoxes();
	}

	var socket;
	var pingIntervalId;

	// function scanSuccess(data, textStatus, jqXHR){
	// 	console.log(data);
	// 	$("#scanbutton").data("state", 0);
	// 	$("#scanbutton").text("Scan");
	// }
	function Connect(){
		console.log("CONNECT");
		socket = new WebSocket("ws://192.168.1.128:8080/socket/");
	}
	Connect();

	socket.onopen = function(){
		console.log("SOCKET OPEN");
		setConnectionUp();
		pingIntervalId = setInterval(sendPing, 5000);
	}
	socket.onclose = function(){
		console.log("SOCKET CLOSED");
		setConnectionDown();
		clearInterval(pingIntervalId);
		setTimeout(Connect, 10);
	}


	socket.onmessage = function(msg){
		if (msg.data != "PONG"){
			var responseStructure = JSON.parse(msg.data);
			console.log(responseStructure);
			if (responseStructure.TYPE == "RESPONSE") {
				if (responseStructure.SUCCESS  == "TRUE"){
					setScannerBusy(responseStructure.INFO);
				} else {
					setScannerAvailable();
				}
			} else if (responseStructure.TYPE == "STATUS"){
				if (responseStructure.SUCCESS  == "TRUE"){
					var filename = responseStructure.FILENAME;
					loadImage(filename);
					setScannerAvailable();
				}

			} else if (responseStructure.TYPE == "SCANNER"){
				if (responseStructure.STATUS == "ACTIVE"){
					setScannerBusy(responseStructure.INFO);
				} else {
					setScannerAvailable();
				}
			} else if (responseStructure.TYPE == "FILES"){
				loadImages(responseStructure.IMAGE_LIST);
			}
			message(msg.data);
		}
	}

	function message(msg){
		console.log(msg);
		$('.messages').append('<p>' + msg + '</p>');
	}

	function sendPing(){
		//console.log("Ping");
		socket.send("PING");
	}

	$("#scanbutton").click(function(){
		if ($(this).data("state") == 0){
			var formObject = {
				"ACTION" : "SCAN",
				"RESOLUTION" : $("#DPISelect").val(),
				"MODE" : $("#ModeSelect").val(),
			};
			console.log(JSON.stringify(formObject));
			socket.send(JSON.stringify(formObject));
			$(this).data("state", 1);
		}
	});

	function loadImages(imageList){
		//Loop image list provided, any new ones are loaded
		$.each(imageList, function(i, val){
			if ($.inArray(val, images) == -1){
				_loadImage(val);
			}
		});
		images = imageList
	}
	function loadImage(image){
		_loadImage(image);
		images.push(image)
	}
	function _loadImage(image){
		//Loop image list provided, any new ones are loaded
		console.log("Loading: "+image);
		var img = new Image();
		img.onload = function () {
			//check height and limit size accordingly
			if (img.height > MAX_IMAGE_HEIGHT){
				ratio = MAX_IMAGE_HEIGHT / img.height;
				console.log(ratio);
				img.height = MAX_IMAGE_HEIGHT;
				img.width = img.width * ratio;
			}
			var li = $( document.createElement('li') ),
				container = $(".outputList ul");
			li.append(img)
			container.prepend(li); 
		};
		img.src = "/scans/" + image;
	}

	function getAllImages() {
		var formObject = {
				"ACTION" : "GET_FILES"
			};
		console.log(JSON.stringify(formObject));
		socket.send(JSON.stringify(formObject));
	}
	$("#reloadButton").click(getAllImages);
	getAllImages();
})
