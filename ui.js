function choose(sample) {
	return function() {
		prompt = document.querySelector('textarea#text');
		prompt.value += sample;
		div = document.querySelector('div#responses');
		div.innerHTML = '';
		calc();
	};
}

function update(json) {
	div = document.querySelector('div#responses');
	for (sample of json['text']) {
		p = document.createElement('p');
		span = document.createElement('span');
		span.className = 'sample';
		span.innerText = sample;
		span.onclick = choose(sample);
		p.append(span)
		div.append(p);
	}
}

function reqListener () {
	text = this.responseText;
	console.log(text)
	items = JSON.parse(text);
	update(items);
}
function calc() {
	div = document.querySelector('div#responses');
	div.innerHTML = '';

	prompt = document.querySelector('textarea#text');

	var oReq = new XMLHttpRequest();
	oReq.open("POST", "http://localhost:8000/predict", true);
	oReq.onload = reqListener;
	oReq.send(prompt.value);

	// testResponse = [" or 'the man of the people.' But his name was not known and there was much suspicion amongst the Egyptians in the land of Egypt.\n\nOn the 13th of October the Egyptian ambassador to", " at the top of his body (with his knees up on his back) and I thought \"that's amazing how they do it, they did it, great job\".\n\n\nWe took it one", "-sama\u2026 We'll have to get to know each other before we leave.\"\n\n\"Are you going to go through with that?\"\n\n\"That won't be true\u2026\"\n\n\"", " that you should be the last person to see the other man or woman. The men in your town will all know this. If possible, be certain that you have the right to bring them into your", ", who had just been awarded his Golden State title at age 27, announced that he was going to enter the NBA draft with his mother.\n\n\"I'm so happy to have the opportunity to", ", the director of the Middle East program at the Centre for Strategic Studies thinktank, was among those who said the Syrian army is likely to succeed the Kurds and Arabs of the northern part of the country", "'s work. He was not prepared for it\u2014he did not think about it. His idea of life took a lot of thinking, a lot of work, and a lot of work. He thought", " and her friend, S.K. Gupta, were attacked in their cars and taken to the University of Delhi police station in Goa. A few hours later, three policemen had been ambushed in", ".\n\nThe same issue was raised several years ago about the presence of marijuana that was a banned commodity that was also consumed by military personnel. In June 2013, in an order issued by the CIA", " was to be transferred to the same unit. In the meantime the \"Jungle Commander\" had already had contact with the group of \"Nexus Ones\".\n\nHe arrived at our camp,"];

	// update(testResponse);
}
