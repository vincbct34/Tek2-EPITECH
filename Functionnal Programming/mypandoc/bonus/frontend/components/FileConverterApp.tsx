'use client';

import { useState } from 'react';
import FileUploader from '@/components/FileUploader';
import PreviewContainer from '@/components/PreviewContainer';
import ConversionControls from '@/components/ConversionControls';
import { Card, CardContent } from '@/components/ui/card';
import { Tabs, TabsContent, TabsList, TabsTrigger } from '@/components/ui/tabs';
import { AlertCircle, CheckCircle2 } from 'lucide-react';
import { Alert, AlertDescription, AlertTitle } from '@/components/ui/alert';

export type FileInfo = {
  content: string;
  type: 'json' | 'xml' | 'markdown' | 'unknown';
};

export type ConversionStatus = 'idle' | 'loading' | 'success' | 'error';

export default function FileConverterApp() {
  const [sourceFile, setSourceFile] = useState<FileInfo | null>(null);
  const [convertedFile, setConvertedFile] = useState<FileInfo | null>(null);
  const [conversionStatus, setConversionStatus] = useState<ConversionStatus>('idle');
  const [error, setError] = useState<string | null>(null);
  const [theTargetFormat, setTargetFormat] = useState<string>('json');

  const handleFileUpload = (file: FileInfo) => {
    setSourceFile(file);
    setConvertedFile(null);
    setConversionStatus('idle');
    setError(null);
  };

  const handleConvertFile = async (targetFormat: string) => {
    if (!sourceFile) return;

    setTargetFormat(targetFormat);

    try {
      setConversionStatus('loading');
      setError(null);

      const response = await convertFile(sourceFile, targetFormat);

      setConvertedFile(response);
      setConversionStatus('success');
    } catch (err) {
      setConversionStatus('error');
      setError(err instanceof Error ? err.message : 'An unknown error occurred');
    }
  };

  return (
    <div className="space-y-6">
      {error && (
        <Alert variant="destructive">
          <AlertCircle className="h-4 w-4" />
          <AlertTitle>Error</AlertTitle>
          <AlertDescription>{error}</AlertDescription>
        </Alert>
      )}

      {conversionStatus === 'success' && (
        <Alert className="bg-green-50 border-green-200 dark:bg-green-950/30 dark:border-green-900">
          <CheckCircle2 className="h-4 w-4 text-green-600 dark:text-green-400" />
          <AlertTitle>Success</AlertTitle>
          <AlertDescription>File converted successfully.</AlertDescription>
        </Alert>
      )}

      <Tabs defaultValue="upload" className="w-full">
        <TabsList className="grid w-full grid-cols-2">
          <TabsTrigger value="upload">Upload</TabsTrigger>
          <TabsTrigger
            value="convert"
            disabled={!sourceFile}
          >
            Convert
          </TabsTrigger>
        </TabsList>

        <TabsContent value="upload" className="space-y-4">
          <Card>
            <CardContent className="pt-6">
              <FileUploader onFileUpload={handleFileUpload} />
            </CardContent>
          </Card>

          {sourceFile && (
            <Card>
              <CardContent className="pt-6">
                <h2 className="text-xl font-semibold mb-4">Source File Preview</h2>
                <PreviewContainer file={sourceFile} />
              </CardContent>
            </Card>
          )}
        </TabsContent>

        <TabsContent value="convert" className="space-y-4">
          {sourceFile && (
            <div className="grid grid-cols-1 lg:grid-cols-2 gap-6">
              <Card>
                <CardContent className="pt-6">
                  <h2 className="text-xl font-semibold mb-4">Source File</h2>
                  <PreviewContainer file={sourceFile} />
                </CardContent>
              </Card>

              <div className="space-y-4">
                <Card>
                  <CardContent className="pt-6">
                    <h2 className="text-xl font-semibold mb-4">Conversion</h2>
                    <ConversionControls
                      sourceType={sourceFile.type}
                      onConvert={handleConvertFile}
                      status={conversionStatus}
                    />
                  </CardContent>
                </Card>

                {convertedFile && (
                  <Card>
                    <CardContent className="pt-6">
                      <h2 className="text-xl font-semibold mb-4">Converted File</h2>
                      <PreviewContainer file={convertedFile} targetFormat={theTargetFormat}/>
                    </CardContent>
                  </Card>
                )}
              </div>
            </div>
          )}
        </TabsContent>
      </Tabs>
    </div>
  );
}

async function convertFile(file: FileInfo, targetFormat: string): Promise<FileInfo> {
  let rawContent;

  if (file.type == "json")
    rawContent = JSON.stringify(JSON.parse(file.content));
  else if (file.type == "markdown") {
    rawContent = file.content.replace(/(\n\s*){2,}/g, '\n');
  } else if (file.type == "xml")
    rawContent = file.content;

  const response = await fetch('http://172.26.28.39:3000/api/convert', {
    method: 'POST',
    headers: {
      "Content-Type": "application/x-www-form-urlencoded",
    },
    body: new URLSearchParams(
      rawContent
        ? { file: rawContent, format: targetFormat }
        : { format: targetFormat }
    ),
  });

  if (!response.ok) {
    const errorMessage = await response.text();
    throw new Error(errorMessage || 'Failed to convert file');
  }

  const convertedFile = await response.json();
  return {
    content: convertedFile,
    type: file.type
  };
}
