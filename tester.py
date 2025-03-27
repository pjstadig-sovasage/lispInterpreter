import unittest
import glob 
import os
from controller import Controller

class TestREPL(unittest.TestCase):    
    TEST_DIR = "testSuite/tests"
    KEY_DIR = "testSuite/keys"
    RES_DIR = "testSuite/results"
    
    def test_files(self):
        self.maxDiff = None
        controller = Controller()
        testFiles = glob.glob(os.path.join(self.TEST_DIR, "*.txt"))
                
        for test in testFiles:
            fileName = os.path.basename(test)
            keyFile = os.path.join(self.KEY_DIR, fileName)
            resFile = os.path.join(self.RES_DIR, fileName)

            with self.subTest(fileName):
                self.assertTrue(os.path.exists(keyFile), f"Key file missing for {fileName}")

                with open(keyFile, 'r', encoding='utf-8') as file:
                    expectedOutput = file.read().strip()
                    
                print(f"\nRunning File: {os.path.normpath(test)}")
                controller.runFile(os.path.normpath(test), False)
                
                with open(resFile, 'r', encoding='utf-8') as file:
                    actualOutput = file.read().strip()

                self.assertEqual(actualOutput, expectedOutput, f"Output mismatch for {fileName}")