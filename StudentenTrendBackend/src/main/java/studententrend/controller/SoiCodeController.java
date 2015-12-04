package studententrend.controller;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.RestController;

import studententrend.model.SoiCode;
import studententrend.model.dao.SoiCodeRepository;

@RestController
public class SoiCodeController {

	@Autowired
	SoiCodeRepository soiCodeRepository;

	@RequestMapping(
			value = "/soicodes", 
			method = RequestMethod.GET,
			headers = "Accept=application/json", 
			produces = {"application/json"})
	@ResponseBody
	public List<SoiCode> soiCodeAll() {
		List<SoiCode> soiCodes = (List<SoiCode>) soiCodeRepository.findAll();
		
		for(SoiCode code : soiCodes){
			System.out.println(code.toString() + '\n');
		}
		
		return soiCodes;
	}
}