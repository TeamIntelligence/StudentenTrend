package studententrend.controller;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.RestController;

import studententrend.model.IscedCode;
import studententrend.model.dao.IscedCodeRepository;

@RestController
public class IscedCodeController {
	@Autowired
	IscedCodeRepository iscedCodeRepository;

	@RequestMapping(
			value = "/iscedcodes", 
			method = RequestMethod.GET,
			headers = "Accept=application/json", 
			produces = {"application/json"})
	@ResponseBody
	public List<IscedCode> iscedCodeAll() {
		List<IscedCode> iscedCodes = (List<IscedCode>) iscedCodeRepository.findAll();
		
		for(IscedCode code : iscedCodes){
			System.out.println(code.toString() + '\n');
		}
		
		return iscedCodes;
	}
	
}
