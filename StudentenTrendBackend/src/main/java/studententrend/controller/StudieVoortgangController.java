package studententrend.controller;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.RestController;

import studententrend.model.StudieVoortgang;
import studententrend.model.dao.StudieVoortgangRepository;

@RestController
@RequestMapping("/studenten")
public class StudieVoortgangController {
	@Autowired
	StudieVoortgangRepository studieVoortgangRepository;

	@RequestMapping(value = "/studievoortgang", method = RequestMethod.GET, headers = "Accept=application/json", produces = {
			"application/json" })
	@ResponseBody
	public List<StudieVoortgang> studieVoortgangAll() {
		List<StudieVoortgang> studieVoortgangen = (List<StudieVoortgang>) studieVoortgangRepository.findAll();

		return studieVoortgangen;
	}

	@RequestMapping(value = "/studievoortgang/{iscedCode}", method = RequestMethod.GET, headers = "Accept=application/json", produces = {
			"application/json" })
	@ResponseBody
	public List<StudieVoortgang> studieVoortgangIsced(@PathVariable("iscedCode") int iscedCode) {
		List<StudieVoortgang> studieVoortgangen = (List<StudieVoortgang>) studieVoortgangRepository.findByIscedCode(iscedCode);

		return studieVoortgangen;
	}

}
