package studententrend.controller;

//@RestController
//public class FileController {

//	@RequestMapping(value = "/files/{file_name}", method = RequestMethod.GET)
//	public void getFile(@PathVariable("file_name") String fileName, HttpServletResponse response) {
//		System.out.println("HELLO");
//		try {
//			// get your file as InputStream
//			InputStream is = new FileInputStream("C:/Users/tim/Dropbox/Project Data Science/Data/Studenten/HBO_WO_gediplomeerden.xlsx");
//			// copy it to response's OutputStream
//			org.apache.commons.io.IOUtils.copy(is, response.getOutputStream());
//			response.flushBuffer();
//		} catch (IOException ex) {
//			System.out.println("Error writing file to output stream. Filename was '{}'" + fileName + ex);
//			throw new RuntimeException("IOError writing file to output stream");
//		}
//	}
//
//}
