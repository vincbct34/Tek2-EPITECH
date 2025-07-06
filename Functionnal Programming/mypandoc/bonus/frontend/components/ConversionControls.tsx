'use client';

import React, { useState } from 'react';
import { Button } from '@/components/ui/button';
import { Select, SelectContent, SelectItem, SelectTrigger, SelectValue } from '@/components/ui/select';
import { ArrowRightLeft, Loader2 } from 'lucide-react';
import { ConversionStatus } from './FileConverterApp';

interface ConversionControlsProps {
  sourceType: string;
  onConvert: (targetFormat: string) => void;
  status: ConversionStatus;
}

export default function ConversionControls({ 
  sourceType, 
  onConvert, 
  status 
}: ConversionControlsProps) {
  const [targetFormat, setTargetFormat] = useState<string>('');
  
  // Determine available target formats based on source type
  const availableFormats = getAvailableFormats(sourceType);
  
  const handleConvert = () => {
    if (targetFormat) {
      onConvert(targetFormat);
    }
  };
  
  return (
    <div className="space-y-6">
      <div className="grid grid-cols-[1fr,auto,1fr] items-center gap-3">
        <div className="border rounded-md p-3 bg-muted/50 text-center">
          <span className="font-medium">{sourceType.toUpperCase()}</span>
        </div>
        
        <div className="flex items-center justify-center">
          <ArrowRightLeft className="h-6 w-6 text-muted-foreground" />
        </div>
        
        <Select value={targetFormat} onValueChange={setTargetFormat}>
          <SelectTrigger>
            <SelectValue placeholder="Select format" />
          </SelectTrigger>
          <SelectContent>
            {availableFormats.map((format) => (
              <SelectItem key={format.value} value={format.value}>
                {format.label}
              </SelectItem>
            ))}
          </SelectContent>
        </Select>
      </div>
      
      <Button 
        className="w-full" 
        disabled={!targetFormat || status === 'loading'}
        onClick={handleConvert}
      >
        {status === 'loading' ? (
          <>
            <Loader2 className="mr-2 h-4 w-4 animate-spin" />
            Converting...
          </>
        ) : (
          'Convert File'
        )}
      </Button>
      
      <div className="text-xs text-muted-foreground mt-2">
        <p>
          This will send your file to our conversion service.
          The file will be processed and converted to the selected format.
        </p>
      </div>
    </div>
  );
}

function getAvailableFormats(sourceType: string) {
  switch (sourceType) {
    case 'json':
      return [
        { value: 'markdown', label: 'Markdown' },
        { value: 'xml', label: 'XML' },
        { value: 'json', label: 'JSON' },
      ];
    case 'xml':
      return [
        { value: 'json', label: 'JSON' },
        { value: 'xml', label: 'XML' },
        { value: 'markdown', label: 'Markdown' },
      ];
    case 'markdown':
      return [
        { value: 'json', label: 'JSON' },
        { value: 'xml', label: 'XML' },
        { value: 'markdown', label: 'Markdown' },
      ];
    default:
      return [];
  }
}